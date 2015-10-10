
/*
 * worker.c
 * Open server socket and wait for hosts to send work to complete
 * Explicitly accepts python code
 */

#include <stdio.h>      /* for printf() and fprintf() */
#include <sys/socket.h> /* for socket(), bind(), and connect() */
#include <arpa/inet.h>  /* for sockaddr_in and inet_ntoa() */
#include <stdlib.h>     /* for atoi() and exit() */
#include <string.h>     /* for memset() */
#include <unistd.h>     /* for close() */
#include <signal.h>     /* for signal() */

#define MAXPENDING 5    /* Maximum outstanding connection requests */

#define KeyMax 5

static void die(const char *message)
{
    perror(message);
    exit(1);
}

static int runPythonCode(char *file,  int clntSock)
{
    ssize_t n;
    pid_t pid;
    static char buf[512];
    int statusCode, i, out[2];

    if (pipe(out) != 0)
        die("Cannot create pipe");

    pid = fork();
    if (pid == -1) {
        die("Cannot create child process");
    } else if (pid > 0) {
        close(out[1]);

        // Receive the output of the child via the pipe and send it to the
        // client over the client socket.
        do {
            n = read(out[0], buf, sizeof(buf));
            if (n > 0) {
                if (send(clntSock, buf, n, 0) != n) {
                    // send() failed.
                    // We log the failure, break out of the loop,
                    // and let the server continue on with the next request.
                    perror("\nsend() failed");
                    break;
                }
            }
        } while(n > 0);

        return 0;
    }

    for(i = 0; i < 3; i++) {
        close(i);
        if (dup(out[1]) != i)
            die("Can't reconnect standard input/output");
    }

    // Replace the current process image with /bin/ls.
    char *args[] = {"python", file, NULL};
    if (execv("/bin/python", args) == -1)
        perror("Cannot execute /bin/ls");
    return 0;
}

// CHANGE: added an additional parameter
void HandleTCPClient(int clntSocket, char *db_filename);

int main(int argc, char *argv[])
{
    int servSock;                    /* Socket descriptor for server */
    int clntSock;                    /* Socket descriptor for client */
    struct sockaddr_in echoServAddr; /* Local address */
    struct sockaddr_in echoClntAddr; /* Client address */
    unsigned short echoServPort;     /* Server port */
    unsigned int clntLen;            /* Length of client address data struct */

    // CHANGE: ignore SIGPIPE so that we don't terminate when we call
    // send() on a disconnected socket.

    if (signal(SIGPIPE, SIG_IGN) == SIG_ERR)
	die("signal() failed");

    // CHANGE: progarm takes two parameters

    if (argc != 3)
    {
        fprintf(stderr, "Usage:  %s <database_file> <Server Port>\n", argv[0]);
        exit(1);
    }

    char *db_filename = argv[1];
    echoServPort = atoi(argv[2]);  /* 2nd arg:  local port */

    /* Create socket for incoming connections */
    if ((servSock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0)
        die("socket() failed");

    /* Construct local address structure */
    memset(&echoServAddr, 0, sizeof(echoServAddr));   // Zero out structure
    echoServAddr.sin_family = AF_INET;                // Internet address family
    echoServAddr.sin_addr.s_addr = htonl(INADDR_ANY); // Any incoming interface
    echoServAddr.sin_port = htons(echoServPort);      // Local port

    /* Bind to the local address */
    if (bind(servSock, (struct sockaddr *)&echoServAddr,
		sizeof(echoServAddr)) < 0)
        die("bind() failed");

    /* Mark the socket so it will listen for incoming connections */
    if (listen(servSock, MAXPENDING) < 0)
        die("listen() failed");

    for (;;) /* Run forever */
    {
        /* Set the size of the in-out parameter */
        clntLen = sizeof(echoClntAddr);

        /* Wait for a client to connect */
        if ((clntSock = accept(servSock, (struct sockaddr *) &echoClntAddr,
                               &clntLen)) < 0)
            die("accept() failed");

        /* clntSock is connected to a client! */

        fprintf(stderr, "\nconnection started from: %s\n",
		inet_ntoa(echoClntAddr.sin_addr));

        HandleTCPClient(clntSock, db_filename);

	fprintf(stderr, "connection terminated from: %s\n",
		inet_ntoa(echoClntAddr.sin_addr));
    }
    /* NOT REACHED */
}

void HandleTCPClient(int clntSocket, char *db_filename)
{
    // CHANGE: this function is what used to be main() in mdb-lookup.c

    /*
     * open the database file specified in the argument
     */

    FILE *fp = fopen(db_filename, "rb"); // open in read, binary mode
    if (fp == NULL)
	die(db_filename);

    /*
     * read all records into memory
     */

    struct List list;
    initList(&list);

    struct MdbRec r;
    struct Node *node = NULL;

    while (fread(&r, sizeof(r), 1, fp) == 1) {

	// allocate memory for a new record and copy into it the one
	// that was just read from the database.
	struct MdbRec *rec = (struct MdbRec *)malloc(sizeof(r));
	if (!rec)
	    die("malloc failed");
	memcpy(rec, &r, sizeof(r));

	// add the record to the linked list.
	node = addAfter(&list, node, rec);
	if (node == NULL)
	    die("addAfter() failed");
    }

    // see if fread() produced error
    if (ferror(fp))
	die(db_filename);

    /*
     * lookup loop
     */

    // CHANGE: wrap the socket with a FILE* using fdopen()
    FILE *input = fdopen(clntSocket, "r");
    if (input == NULL)
	die("fdopen failed");

    char line[1000];
    char key[KeyMax + 1];

    char out_buf[1000];
    int len;
    int res;

    // CHANGE: we don't do lookup prompt in this socket version of the
    // porgram as it interferes with detecting the end of result using
    // a blank line.

    while (fgets(line, sizeof(line), input) != NULL) {

	// must null-terminate the string manually after strncpy().
	strncpy(key, line, sizeof(key) - 1);
	key[sizeof(key) - 1] = '\0';

	// if newline is there, remove it.
	size_t last = strlen(key) - 1;
	if (key[last] == '\n')
	    key[last] = '\0';

	// traverse the list, printing out the matching records
	struct Node *node = list.head;
	int recNo = 1;
	while (node) {
	    struct MdbRec *rec = (struct MdbRec *)node->data;
	    if (strstr(rec->name, key) || strstr(rec->msg, key)) {
		// CHANGE: we changed printf() into sprintf() & send()
		len = sprintf(out_buf, "%4d: {%s} said {%s}\n", recNo, rec->name, rec->msg);
		if ((res = send(clntSocket, out_buf, len, 0)) != len) {
		    perror("send() failed");
		    break;
		}
	    }
	    node = node->next;
	    recNo++;
	}

	// CHANGE: we changed printf() into sprintf() & send()
	// CHANGE: send a blank line to indicate the end of search result
	len = sprintf(out_buf, "\n");
	if ((res = send(clntSocket, out_buf, len, 0)) != len)
	    perror("send() failed");
    }

    // CHANGE: check 'input' rather than 'stdin'
    //
    // see if fgets() produced error
    if (ferror(input)) {
	perror("fgets() failed");
    }

    /*
     * clean up and quit
     */

    // free all the records
    traverseList(&list, &free);

    removeAllNodes(&list);

    // close the database file
    fclose(fp);

    // CHANGE: close the socket by closing the FILE* wrapper
    fclose(input);
}
