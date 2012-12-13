ocamlopt := ocamlopt
ocamlc := ocamlc
ocamldoc := ocamldoc
ocamldep := ocamldep
ocamllex := ocamllex
ocamlyacc := ocamlyacc

ocamlflags := 

# Order of this is significant!!!
obj_bases := error st parse lex translate output main
byte_objs := $(addsuffix .cmo,$(obj_bases)) 
byte_interfaces := $(addsuffix .cmi,$(obj_bases)) 
#native_objs := $(addsuffix .cmx,$(obj_bases))

lex_bases := lex.mll
yacc_bases := parse.mly

program := h2asm
byte_prog := $(program)
opt_prog := $(program).opt

all: $(byte_prog)
#all: $(byte_prog) $(opt_prog)

depend.mk: $(wildcard *.ml *.mli) lex.ml parse.ml
	$(ocamldep) $(ocamlflags) *.ml > depend.mk
include depend.mk

$(byte_prog): $(byte_objs)
	$(ocamlc) $(ocamlflags) -o $(program) $(byte_objs)

%.cmi: %.mli
	$(ocamlc) $(ocamlflags) -c $<

%.cmo: %.ml %.cmi
	$(ocamlc) $(ocamlflags) -c $<

lex.ml: $(lex_bases)
	$(ocamllex) $<

parse.ml: $(yacc_bases)
	$(ocamlyacc) -v $<

clean:
	-rm -f -- $(obj_intfs)
	-rm -f -- $(byte_objs) $(native_objs)
	-rm -f -- $(byte_prog) $(native_prog)
	-rm -f -- $(byte_objs) $(native_prog_objs)
	-rm -f -- depend.mk
	-rm -f -- *.o *.cmi *.cmo
	-rm -f -- *~
	-rm -f -- *.a
	-rm -f -- lex.ml
	-rm -f -- parse.ml parse.mli

