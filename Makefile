BIN:=CloogleServer search builddb
PRJ:=$(addsuffix .prj,$(BIN))
DB=types.db
MAN:=builddb.1 # Others don't have --help/--version # $(addsuffix .1,$(BIN))
CPM:=cpm
SED:=sed

.SECONDARY: $(PRJ)
.PHONY: all

all: $(BIN) $(DB)

man: $(MAN)

%.1: %
	help2man -N ./$< > $@

%: %.prj
	$(CPM) $< \
		| grep -v "^Analyzing" \
		| grep -v "^Warning: Unable to setup directory cache"

%.prj:
	$(CPM) project $(basename $@) create
	$(SED) -i 's/\(Target:[ \t]\+\)StdEnv/\1CleanPlatform/' $@
	$(SED) -i 's/[ \t]\+Path:[ \t]\+{Project}/&\n&\/CleanLevenshtein\n&\/CleanTypeUnifier\n&\/CleanTypeUnifier\/clean-compiler\/main\/\n&\/CleanTypeUnifier\/clean-compiler\/frontend\/\n&\/CleanTypeUnifier\/clean-compiler\/backend\/\n&\/CleanTypeUnifier\/clean-compiler\/main\/Unix\//' $@
	$(SED) -i 's/\([ \t]\+Path:[ \t]\+\){Project}$$/&\n\1{Application}\/lib\/ArgEnv\/\n\1{Application}\/lib\/TCPIP\//' $@
	$(SED) -i 's/\($(basename $@)\).exe/\1/' $@
	$(SED) -i 's/\(Output:[ \t]\+\)ShowConstructors/\1NoConsole/' $@
	$(SED) -i 's/\(HeapSize:[ \t]\+\)2097152/\110485760/' $@

$(DB): builddb
	./builddb > $(DB)

clean:
	$(RM) -r 'Clean System Files' $(BIN) $(PRJ) $(MAN) $(DB)
