BIN:=CloogleServer builddb
DB=types.json
MAN:=builddb.1 # Others don't have --help/--version # $(addsuffix .1,$(BIN))
CLM:=clm
CLMFLAGS:=-dynamics -h 40M -nr -nt\
	-I $$CLEAN_HOME/lib/clean-platform/OS-Independent\
	-I $$CLEAN_HOME/lib/clean-platform/OS-Linux-64\
	-I $$CLEAN_HOME/lib/clean-platform/OS-Linux\
	-I $$CLEAN_HOME/lib/clean-platform/OS-Posix\
	-I $$CLEAN_HOME/lib/ArgEnv\
	-I $$CLEAN_HOME/lib/Dynamics\
	-I $$CLEAN_HOME/lib/Generics\
	-I $$CLEAN_HOME/lib/StdLib\
	-I $$CLEAN_HOME/lib/TCPIP\
	-I CleanTypeUnifier\
	-I CleanLevenshtein\
	-I CleanPrettyPrint\
	-I clean-compiler/frontend\
	-I clean-compiler/backend\
	-I clean-compiler/main\
	-I clean-compiler/main/Unix

.SECONDARY: $(PRJ)
.PHONY: all clean distclean

all: $(BIN) $(DB)

clean-compiler:
	svn checkout https://svn.cs.ru.nl/repos/clean-compiler/branches/itask/ clean-compiler
	$(MAKE) -C clean-compiler/main/Unix
	$(MAKE) -C clean-compiler/backendC/CleanCompilerSources -f Makefile.linux64
	$(LN) -s ../../backendC/CleanCompilerSources/backend.a clean-compiler/backend/Clean\ System\ Files/backend_library

man: $(MAN)

%.1: %
	help2man -N ./$< > $@

$(BIN): clean-compiler .FORCE
	$(CLM) $(CLMFLAGS) $@ -o $@

$(DB): builddb
	./$< > $(DB)

clean:
	$(RM) -r 'Clean System Files' $(BIN) $(MAN) $(DB)

distclean: clean
	$(RM) -r clean-compiler

.FORCE:
