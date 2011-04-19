all:
	happy -gca ParJavalette.y
	alex -g LexJavalette.x
	latex DocJavalette.tex; dvips DocJavalette.dvi -o DocJavalette.ps
	ghc --make TestJavalette.hs -o TestJavalette
	ghc --make Javalette.hs -o jlc
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocJavalette.ps
distclean: clean
	-rm -f DocJavalette.* LexJavalette.* ParJavalette.* LayoutJavalette.* SkelJavalette.* PrintJavalette.* TestJavalette.* AbsJavalette.* TestJavalette ErrM.* SharedString.* Javalette.dtd XMLJavalette.* Makefile*

jlc:
	ghc --make Javalette.hs -o jlc
