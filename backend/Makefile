BIN:=CloogleServer builddb
PRJ:=$(addsuffix .prj,$(BIN))
DB=types.json
MAN:=builddb.1 # Others don't have --help/--version # $(addsuffix .1,$(BIN))
CPM:=cpm
SED:=sed

.SECONDARY: $(PRJ)
.PHONY: all clean distclean

all: $(BIN) $(DB)

clean-compiler:
	svn checkout https://svn.cs.ru.nl/repos/clean-compiler/branches/itask/ clean-compiler
	$(MAKE) -j -C clean-compiler/main/Unix
	$(MAKE) -j -C clean-compiler/backendC/CleanCompilerSources -f Makefile.linux64
	ln -s ../../backendC/CleanCompilerSources/backend.a clean-compiler/backend/Clean\ System\ Files/backend_library

man: $(MAN)

%.1: %
	help2man -N ./$< > $@

$(BIN): %: %.prj clean-compiler
	$(CPM) $< \
		| grep -v "^Analyzing" \
		| grep -v "^Warning: Unable to setup directory cache"

%.prj:
	$(CPM) project $(basename $@) create
	$(SED) -i 's/\(Target:[ \t]\+\)StdEnv/\1CleanPlatform/' $@
	$(SED) -i 's/[ \t]\+Path:[ \t]\+{Project}/&\n&\/CleanLevenshtein\n&\/CleanPrettyPrint\n&\/CleanTypeUnifier\n&\/clean-compiler\/main\/\n&\/clean-compiler\/frontend\/\n&\/clean-compiler\/backend\/\n&\/clean-compiler\/main\/Unix\//' $@
	$(SED) -i 's/\([ \t]\+Path:[ \t]\+\){Project}$$/&\n\1{Application}\/lib\/ArgEnv\/\n\1{Application}\/lib\/TCPIP\//' $@
	$(SED) -i 's/\($(basename $@)\).exe/\1/' $@
	$(SED) -i 's/\(Output:[ \t]\+\)ShowConstructors/\1NoConsole/' $@
	$(SED) -i 's/\(HeapSize:[ \t]\+\)2097152/\141943040/' $@

$(DB): builddb
	./$< > $(DB)

clean:
	$(RM) -r 'Clean System Files' $(BIN) $(PRJ) $(MAN) $(DB)

distclean: clean
	$(RM) -r clean-compiler
