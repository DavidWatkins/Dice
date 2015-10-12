# Makefile for Part 1 of HW3a: Multi-process web server
#
# Date: 2015 -- 02 -- 21

CC = gcc

INCLUDES =
CFLAGS = -g -Wall $(INCLUDES)

LDFLAGS = -g

.PHONY: all
all: clean worker host

worker: worker.o

worker.o: worker.c

host: host.o

host.o: host.c

.PHONY: clean
clean:
	rm -rf *.o *~ worker host
