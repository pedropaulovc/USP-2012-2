CXX=g++
CXXFLAGS=-Wall -ansi -pedantic -O2 -Wno-unused-result -lm
DEPS      = servidor_main.h servidor_gerente.h servidor_tcp.h servidor_udp.h util_tcp.h util_udp.h cliente_main.h
OBJ       = util_tcp.o util_udp.o
OBJSERVER = servidor_main.o servidor_gerente.o servidor_tcp.o servidor_udp.o
OBJCLIENT = cliente_main.o

all: servidor

%.o: %.cpp $(DEPS)
	$(CXX) -c -o $@ $< $(CFLAGS)

servidor: $(OBJSERVER) $(OBJ)
	$(CXX) -o $@ $^ $(CFLAGS)

cliente: $(OBJCLIENT) $(OBJ)
	$(CXX) -o $@ $^ $(CFLAGS)

clean:
	rm -f *.o servidor cliente
