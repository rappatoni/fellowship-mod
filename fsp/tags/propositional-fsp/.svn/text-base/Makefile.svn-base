# Florent Kirchner			-- LogiCal
# Makefile


# Variable definition

CMO=ast.cmo lexer.cmo parser.cmo print.cmo commands.cmo interpreter.cmo main.cmo
GENERATED = lexer.ml parser.ml parser.mli 
BIN=fsp
FLAGS=
CAML=ocamlc $(FLAGS)
LIBS= #needed for Unix function calls (but try using Sys instead) unix.cma 

# Definition of the "all" target 

all: $(BIN)

$(BIN):$(CMO)
	$(CAML) -o $(BIN) $(LIBS) $(CMO)

# Custom targets

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly  

.mli.cmi:
	$(CAML) -c  $<

.ml.cmo:
	$(CAML) -c  $<

.mll.ml:
	ocamllex $<

.mly.ml:
	ocamlyacc -v $<

.mly.mli:
	ocamlyacc -v $<

clean:
	rm -f *.cm[io] *.o *~ $(BIN) $(GENERATED) parser.output

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend



