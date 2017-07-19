BIN:=CloogleServer builddb
DB=types.json
MAN:=builddb.1 # Others don't have --help/--version # $(addsuffix .1,$(BIN))
CLM:=clm
CLMFLAGS:=-h 250M -nr -nt\
	-I $$CLEAN_HOME/lib/ArgEnv\
	-I $$CLEAN_HOME/lib/Generics\
	-I $$CLEAN_HOME/lib/TCPIP\
	-I Cloogle\
	-I Cloogle/libcloogle\
	-I Cloogle/clean-platform/src/libraries/OS-Independent\
	-I Cloogle/clean-platform/src/libraries/OS-Independent/Deprecated/StdLib\
	-I Cloogle/clean-platform/src/libraries/OS-Linux-64\
	-I Cloogle/clean-platform/src/libraries/OS-Linux\
	-I Cloogle/clean-platform/src/libraries/OS-Posix\
	-I Cloogle/CleanTypeUnifier\
	-I Cloogle/CleanPrettyPrint\
	-I Cloogle/compiler-patch\
	-I clean-compiler/frontend\
	-I clean-compiler/backend\
	-I clean-compiler/main\
	-I clean-compiler/main/Unix

.PHONY: all clean distclean

all: $(BIN) $(DB)

clean-compiler:
	svn checkout -r 2796 https://svn.cs.ru.nl/repos/clean-compiler/branches/itask/ clean-compiler
	$(MAKE) -j -C clean-compiler/main/Unix
	$(MAKE) -j -C clean-compiler/backendC/CleanCompilerSources -f Makefile.linux64
	ln -s ../../backendC/CleanCompilerSources/backend.a clean-compiler/backend/Clean\ System\ Files/backend_library

man: $(MAN)

%.1: %
	help2man -N ./$< > $@

$(BIN): clean-compiler .FORCE
	$(CLM) $(CLMFLAGS) $@ -o $@

$(DB): builddb
	./$< -s 10M -h 1000M > $(DB)

clean:
	$(RM) -r 'Clean System Files' $(BIN) $(MAN) $(DB)

distclean: clean
	$(RM) -r clean-compiler

.FORCE:
