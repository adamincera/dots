CC = gcc

CFLAGS = -Wall 

main: main.o graph.o node.o

main.o: main.c graph.h node.h

graph.o: graph.h node.h graph.c

node.o: node.h node.c

test: test.o translations.o

translations.o: translations.c translations.h

test.o: translations.h test.c

.PHONY: clean
clean:
	rm -f *.o main test

