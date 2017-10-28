BIN:=CloogleServer builddb
DB=types.json
MAN:=builddb.1 # Others don't have --help/--version # $(addsuffix .1,$(BIN))
CLM:=clm
CLMFLAGS:=-h 250M -nr -nt -nortsopts\
	-I $$CLEAN_HOME/lib/ArgEnv\
	-I $$CLEAN_HOME/lib/Generics\
	-I $$CLEAN_HOME/lib/TCPIP\
	-I $$CLEAN_HOME/lib/Platform\
	-I $$CLEAN_HOME/lib/Platform/Deprecated/StdLib\
	-I Cloogle\
	-I Cloogle/libcloogle\
	-I Cloogle/CleanTypeUnifier\
	-I Cloogle/CleanPrettyPrint
ifeq "$(shell expr `gcc -dumpversion | cut -f1 -d.` \>= 6)" "1"
	CLMFLAGS+=-l -no-pie
endif

.PHONY: all clean distclean

all: $(BIN) $(DB)

clean-compiler:
	svn export -r 2838 https://svn.cs.ru.nl/repos/clean-compiler/branches/itask/ clean-compiler
	cd clean-compiler; for f in ../Cloogle/compiler-patch/*.patch; do patch -p1 < "$$f"; done
	mkdir -p clean-compiler/main/Unix/Clean\ System\ Files
	$(MAKE) -j -C clean-compiler/main/Unix
	$(MAKE) -j -C clean-compiler/backendC/CleanCompilerSources -f Makefile.linux64
	ln -s ../../backendC/CleanCompilerSources/backend.a clean-compiler/backend/Clean\ System\ Files/backend_library

man: $(MAN)

%.1: %
	help2man -N ./$< > $@

$(BIN):
	$(CLM) $(CLMFLAGS) $@ -o $@

builddb: clean-compiler
	$(CLM) $(CLMFLAGS) -s 10M -h 1000M \
		-I clean-compiler/frontend\
		-I clean-compiler/backend\
		-I clean-compiler/main\
		-I clean-compiler/main/Unix\
		$@ -o $@

$(DB): builddb
	./$< > $(DB)

clean:
	$(RM) -r **/Clean\ System\ Files $(BIN) $(MAN) $(DB)

distclean: clean
	$(RM) -r clean-compiler
