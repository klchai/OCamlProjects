CC = ocamlc

EXEC = tp4

all: ${EXEC}

tp4: image.cmo tp4.cmo
 $(CC) -o $@ graphics.cma $< tp4.ml

tp4.cmo: tp4.ml
 $(CC) -c $^

image.cmi: image.mli
 $(CC) -c $^

image.cmo: image.ml image.cmi
 $(CC) -c $<