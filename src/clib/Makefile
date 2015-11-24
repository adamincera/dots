CC = gcc

CFLAGS = -Wall 

main: main.o graph.o node.o

main.o: main.c graph.h node.h

graph.o: graph.h node.h graph.c

node.o: node.h node.c

.PHONY: clean
clean:
	rm -f *.o main
