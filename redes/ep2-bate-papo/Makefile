CXX=g++
CXXFLAGS=-Wall -ansi -pedantic -O2 -Wno-unused-result -lm
DEPS      = cliente_main.h
OBJ       = 
OBJSERVER = servidor.o
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
