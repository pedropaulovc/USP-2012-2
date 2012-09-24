CXX=g++
CXXFLAGS=-Wall -ansi -pedantic -O2 -Wno-unused-result -lm
DEPS      = cliente.h servidor.h
OBJ       = 
OBJSERVER = servidor.o
OBJCLIENT = cliente.o

all: servidor cliente

%.o: %.cpp $(DEPS)
	$(CXX) -c -o $@ $< $(CXXFLAGS)

servidor: $(OBJSERVER) $(OBJ)
	$(CXX) -o $@ $^ $(CFLAGS)

cliente: $(OBJCLIENT) $(OBJ)
	$(CXX) -o $@ $^ $(CFLAGS)

clean:
	rm -f *.o servidor cliente
