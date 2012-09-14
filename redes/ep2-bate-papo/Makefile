CXX=g++
CXXFLAGS=-Wall -ansi -pedantic -O2 -Wno-unused-result -lm -std=gnu99 -lrt
DEPS = servidor.h cliente.h
OBJSERVER = main_servidor.o servidor.o
OBJCLIENT = main_cliente.o cliente.o

all: servidor cliente

%.o: %.cpp $(DEPS)
	$(CXX) -c -o $@ $< $(CFLAGS)

servidor: $(OBJSERVER)
	gcc -o $@ $^ $(CFLAGS)

cliente: $(OBJCLIENT)
	gcc -o $@ $^ $(CFLAGS)

clean:
	rm -f *.o servidor cliente
