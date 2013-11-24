EXAMPLES = examples/grep.hs
HFILES = RegexState.hs $(EXAMPLES)
IFILES = $(HFILES:.hs=.hi)
OFILES = $(HFILES:.hs=.o)
BFILES = $(EXAMPLES:.hs=)

HC = ghc
CFLAGS = -Wall

default: regexer grep

clean:
	rm -rf $(IFILES) $(OFILES) $(BFILES)

regexer: RegexState.hs
	$(HC) $(CFLAGS) -c $<

grep: examples/grep.hs regexer
	$(HC) $(CFLAGS) $<
