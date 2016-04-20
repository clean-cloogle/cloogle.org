BIN:=CloogleServer search builddb
PRJ:=$(addsuffix .prj,$(BIN))
CPM:=cpm
SED:=sed

.SECONDARY: $(PRJ)
.PHONY: all

all: $(BIN)

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

clean:
	$(RM) -r 'Clean System Files' $(BIN) $(PRJ)
