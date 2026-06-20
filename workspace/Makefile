ENTRY=parser_main.ss
EXE=parser

SOURCES=parse_types.ss trace_utils.ss parser_library.ss parser_main.ss

.PHONY: run compile rebuild-run run-compiled clean

run:
	gxi $(ENTRY)

compile:
	gxc $(SOURCES)
	gxc -exe -o $(EXE) $(ENTRY)

run-compiled:
	./$(EXE)

rebuild-run: compile
	./$(EXE)

clean:
	rm -f $(EXE)
	rm -f *.o
	rm -f *.ssi
	rm -f *.scm
	rm -f *.c
