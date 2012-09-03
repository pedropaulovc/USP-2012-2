CC=gcc
CFLAGS=-Wall -ansi -pedantic -O2 -Wno-unused-result -lm -std=gnu99 -lrt
DEPS = servidor_http.h
OBJ = main.o servidor_http.o

all: ep1-pedro

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

ep1-pedro: $(OBJ)
	gcc -o $@ $^ $(CFLAGS)

clean:
	rm -f *.o ep1-pedro
