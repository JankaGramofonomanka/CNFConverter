all:
	cd src; ghc --make Main.hs -o tocnf
	mv src/tocnf tocnf

clean:
	-rm -f src/FromBNFC/*.log src/FromBNFC/*.aux src/FromBNFC/*.hi src/FromBNFC/*.o src/FromBNFC/*.dvi
	-rm -f src/MyCode/*.log src/MyCode/*.aux src/MyCode/*.hi src/MyCode/*.o src/MyCode/*.dvi
	-rm -f src/Test/*.log src/Test/*.aux src/Test/*.hi src/Test/*.o src/Test/*.dvi
	-rm -f src/*.log src/*.aux src/*.hi src/*.o src/*.dvi
	-rm -f tocnf



