CC=gcc
CFLAGS=-Wall -ansi -pedantic -O2 -Wno-unused-result -lm -std=gnu99 -lrt
DEPS = servidor_http.h
OBJ = main.o servidor_http.o

all: servidor

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

servidor: $(OBJ)
	gcc -o $@ $^ $(CFLAGS)

clean:
	rm -f *.o servidor
