CC = gcc
CFLAGS = -Wall -pedantic -I../dist

DIST_SRCS = ../dist/*.c
GEN_EXE = ../gen/summary.exe
LAMBDA_SM = lambda.sm

all: out

support.o: $(DIST_SRCS)
	$(CC) $(CFLAGS) $^ -r -o $@

lambda.c: $(LAMBDA_SM)
	dune exec $(GEN_EXE) $(LAMBDA_SM)

out: main.c lambda.c support.o
	$(CC) $(CFLAGS) main.c lambda.c support.o -o out

run: out
	./out

clean:
	rm -f support.o out lambda.c

.PHONY: all run clean
