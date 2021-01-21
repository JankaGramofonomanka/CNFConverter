all:
	happy -gca FromBNFC/ParQFBF.y
	alex -g FromBNFC/LexQFBF.x
	ghc --make FromBNFC/TestQFBF.hs -o FromBNFC/TestQFBF

clean:
	-rm -f FromBNFC/*.log FromBNFC/*.aux FromBNFC/*.hi FromBNFC/*.o FromBNFC/*.dvi

distclean: clean
	-rm -f FromBNFC/DocQFBF.* FromBNFC/LexQFBF.* FromBNFC/ParQFBF.* FromBNFC/LayoutQFBF.* FromBNFC/SkelQFBF.* FromBNFC/PrintQFBF.* FromBNFC/TestQFBF.* FromBNFC/AbsQFBF.* FromBNFC/TestQFBF FromBNFC/ErrM.* FromBNFC/SharedString.* FromBNFC/ComposOp.* FromBNFC/QFBF.dtd FromBNFC/XMLQFBF.* Makefile*
		-rmdir -p FromBNFC/

