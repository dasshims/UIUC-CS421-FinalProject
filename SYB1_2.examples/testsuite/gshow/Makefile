all:
	rm -f tmp.stdout
	ghc --make -i../.. -i.. Main.hs
	./Main > tmp.stdout
	diff tmp.stdout test.stdout

clean:
	rm -f *.hi *.o *~ tmp.stdout Main main.exe