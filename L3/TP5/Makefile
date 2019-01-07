CC=ocamlc
CMO_DEP=t9.cmo keypad_cli.cmo keypad_gui.cmo
CMI_DEP=t9.cmi keypad_cli.cmi keypad_gui.cmi

%.cmi : %.mli
	$(CC) -c $<

%.cmo : %.ml %.cmi
	$(CC) -c $<

phone : phone.ml $(CMO_DEP) $(CMI_DEP)
	$(CC) -o $@ graphics.cma $(CMO_DEP) $<

clean :
	rm -f *.cmi *.cmo phone
