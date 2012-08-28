build:
	hsc2hs ParserGen/Common/Atoi.hsc
	ghc --make -Wall -O2 -fforce-recomp -i../src ParserGen/Common/atoi.c BenchmarkSuite.hs
