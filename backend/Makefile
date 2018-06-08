SHELL=/bin/bash
BIN:=CloogleServer builddb
DB=types.json
MAN:=builddb.1 # Others don't have --help/--version # $(addsuffix .1,$(BIN))
CLM:=clm
CLMFLAGS:=-h 250M -nr -nortsopts\
	-I $$CLEAN_HOME/lib/ArgEnv\
	-I $$CLEAN_HOME/lib/TCPIP\
	-I $$CLEAN_HOME/lib/Platform\
	-I $$CLEAN_HOME/lib/Platform/Deprecated/StdLib\
	-I Cloogle\
	-I Cloogle/libcloogle\
	-I Cloogle/CleanTypes\
	-I Cloogle/CleanPrettyPrint
ifeq "$(shell expr `gcc -dumpversion | cut -f1 -d.` \>= 6)" "1"
	CLMFLAGS+=-l -no-pie
endif

.PHONY: all clean distclean

all: $(BIN) $(DB)

man: $(MAN)

%.1: %
	help2man -N ./$< > $@

CloogleServer:
	$(CLM) $(CLMFLAGS) $@ -o $@
	shopt -s globstar
	touch **/*.[di]cl
	$(CLM) $(CLMFLAGS) $@ -o $@

builddb: Cloogle/clean-compiler
	$(CLM) $(CLMFLAGS) -s 50M -h 750M -gcm\
		-I Cloogle/clean-compiler/frontend\
		-I Cloogle/clean-compiler/backend\
		-I Cloogle/clean-compiler/main\
		-I Cloogle/clean-compiler/main/Unix\
		$@ -o $@

Cloogle/clean-compiler:
	$(MAKE) -C Cloogle clean-compiler

$(DB): builddb
	./$< > $(DB)

clean:
	$(RM) -r **/Clean\ System\ Files $(BIN) $(MAN) $(DB)

distclean: clean
	$(RM) -r clean-compiler
