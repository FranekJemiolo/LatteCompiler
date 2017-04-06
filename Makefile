all:
	cd src && happy -gca ParLatte.y
	cd src && alex -g LexLatte.x
	cd src && ghc --make TestLatte.hs -o latc_llvm
	cp src/latc_llvm .
	cd lib && clang -emit-llvm -S runtime.c
	cd lib && llvm-as runtime.ll

clean:
	cd src && rm -f *.log *.aux *.hi *.o *.dvi latc_llvm
	rm -f latc_llvm

distclean: clean
	cd src && rm -f DocLatte.* LexLatte.* ParLatte.* LayoutLatte.* SkelLatte.* PrintLatte.* TestLatte.* AbsLatte.* TestLatte ErrM.* SharedString.* ComposOp.* latte.dtd XMLLatte.* Makefile*
	

