
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

#define BUF_SIZE 4096

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

int initializeConnection(char* argv) {
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

  // CHANGE: progarm takes one parameter
  echoServPort = atoi(argv[1]);  /* 2nd arg:  local port */

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

    HandleTCPClient(clntSock, "test.py");

    fprintf(stderr, "connection terminated from: %s\n",
    inet_ntoa(echoClntAddr.sin_addr));
  }
  /* NOT REACHED */
}

void HandleTCPClient(int inSocket, int outSocket, char *filename)
{
  char buf[BUF_SIZE];

  //Define input fd's
  FILE *in, *outputFile;
  if ((in = fdopen(inSocket, "r")) == NULL) die("fdopen failed");
  if ((outputFile = fopen(filename, "wb")) == NULL) die("can't open output file");

  //Read in file from socket:
  size_t n;
  while ((n = fread(buf, 1, sizeof(buf), in)) > 0) {
    if (fwrite(buf, 1, n, outputFile) != n) die("fwrite failed");
  }
  // fread() returns 0 on EOF or on error
  // so we need to check if there was an error.
  if (ferror(in) || ferror(outputFile)) die("fread failed");
  fclose(outputFile);

  //Run program and send output to out
  runPythonCode(filename, outSocket);

  //Delete file
  remove(filename);
}

int main(int argc, char *argv[])
{
  if (argc != 2)
  {
    fprintf(stderr, "Usage:  %s <Server Port>\n", argv[0]);
    exit(1);
  }
  initializeConnection(argv);
}
