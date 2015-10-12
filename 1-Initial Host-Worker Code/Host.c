
/*
* worker.c
* Will attempt to open a connection with the provided client info and execute
* the associated python script on the worker. Prints out the result from
* the worker
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <netdb.h>

#define BUF_SIZE 4096

static void die(const char *msg) {
  perror(msg);
  exit(1);
}

static void printUsage() {
  fprintf(stderr, "usage: host <worker ip> <worker port> <python script name>\n");
  fprintf(stderr, "   ex) host 127.0.0.1 8080 test.py\n");
  exit(1);
}

static int handleFileRequest(const char *filename, int clntSock)
{
    FILE *fp = NULL;

    fp = fopen(filename, "rb");
    if (fp == NULL) {
        die(filename);
    }

    // send the file
    size_t n;
    char buf[BUF_SIZE];
    while ((n = fread(buf, 1, sizeof(buf), fp)) > 0) {
        printf("%s", buf);
        if (send(clntSock, buf, n, 0) != n) {
            perror("\nsend() failed");
            break;
        }
    }
    if (ferror(fp))
        perror("fread failed");
    // clean up
    if (fp)
        fclose(fp);

    return 0;
}

int initializeClient(char* serverName, char* serverPort) {
  char* serverIP;
  int sock;
  struct sockaddr_in serverAddr;
  struct hostent *he;

  // get server ip from server name
  if ((he = gethostbyname(serverName)) == NULL) {
    die("gethostbyname failed");
  }
  serverIP = inet_ntoa(*(struct in_addr *)he->h_addr);

  // create socket
  if ((sock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
    die("socket failed");
  }

  // construct server address
  memset(&serverAddr, 0, sizeof(serverAddr));
  serverAddr.sin_family = AF_INET;
  serverAddr.sin_addr.s_addr = inet_addr(serverIP);
  unsigned short port = atoi(serverPort);
  serverAddr.sin_port = htons(port);

  // connect
  if (connect(sock, (struct sockaddr *)&serverAddr, sizeof(serverAddr)) < 0) {
    die("connect failed");
  }
  return sock;
}

int main(int argc, char **argv) {

  char *filePath;
  char buf[BUF_SIZE];

  if (argc != 4) {
    printUsage();
  }

  // parse args
  filePath = argv[3];

  int sock = initializeClient(argv[1], argv[2]);

  handleFileRequest(filePath, sock);

  // wrap the socket with a FILE* so that we can read the socket using fgets()
  FILE *fd;
  if ((fd = fdopen(sock, "r")) == NULL) {
    die("fdopen failed");
  }

  size_t n;
  while ((n = fread(buf, 1, sizeof(buf), fd)) > 0) {
    printf("%s", buf);
    // if (fwrite(buf, 1, n, outputFile) != n)
    // die("fwrite failed");
  }
  if (ferror(fd))
  die("fread failed");

  // closing fd closes the underlying socket as well.
  fclose(fd);

  return 0;
}
