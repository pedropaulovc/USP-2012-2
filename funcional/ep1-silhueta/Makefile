ERLC=erlc
OBJ = silhueta.beam matrix.beam
TOP = silhueta

all: compila

executa: compila
	erl -noshell -s $(TOP) main

compila: $(TOP)

%.beam: %.erl
	$(ERLC) $<

$(TOP): $(OBJ)

clean:
	rm -f *.beam $(TOP)
