CC = gcc

CFLAGS = -Wall 

CFILES= graph.c node.c list.c dict.c

main: main.o graph.o node.o dict.o

main.o: main.c graph.h node.h

graph.o: graph.h node.h graph.c

node.o: node.h node.c

test: test.o list.o dict.o graph.o node.o

list.o: list.c list.h

test.o: list.h test.c

dict.o: dict.c dict.h

.PHONY: objects
objects:
	$(CC) -c $(CFILES)

.PHONY: library
library: objects
	ar -cvq libdots.a *.o

.PHONY: clean
clean:
	rm -f *.o main test *.a

