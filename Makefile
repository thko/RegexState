HFILES = RegexState.hs
IFILES = $(HFILES:.hs=.hi)
OFILES = $(HFILES:.hs=.o)

HC = ghc
CFLAGS = -Wall

default: RegexState.hs
	$(HC) $(CFLAGS) -c $<
