all:
	runghc test.hs --all | tee results.txt

clean:
	-rm -f out/*.out
	-rm -f out/*.compilestats

newtime: newtime1 newtime2 newtime3


newtime1: 
	-rm out/TestSelectIntWTree.*.out
	runghc time.hs TestSelectIntWTree
	runghc time.hs TestSelectIntWTree
	runghc time.hs TestSelectIntWTree
	grep ';' out/TestSelectIntWTree.*.out

newtime2: 
	-rm out/TestGEq.*.out
	runghc time.hs TestGEq
	runghc time.hs TestGEq
	runghc time.hs TestGEq
	grep ';' out/TestGEq.*.out

newtime3: 
	-rm out/TestRmWeights.*.out
	runghc time.hs TestRmWeights
	runghc time.hs TestRmWeights
	runghc time.hs TestRmWeights
	grep ';' out/TestRmWeights.*.out


time: Hand/time LIGD/time Spine/time EMGM/time SYB1_2/time RepLib/time SmashA/time PolyP/time syb3/time Uniplate/time 

GHCFLAGS = -O2
# GHCFLAGS =

%/time: %
	-rm TestEfficiency.o TestEfficiency.hi
	(cd $*; ghc ${GHCFLAGS} --make -i.. ../TestEfficiency.lhs -o TestEfficiency)
	(cd $*; ./TestEfficiency > time.`date +%Y%m%d%H%M%S`)
	(cd $*; ./TestEfficiency > time.`date +%Y%m%d%H%M%S`)
	(cd $*; ./TestEfficiency > time.`date +%Y%m%d%H%M%S`)
	(cd $*; ./TestEfficiency > time.`date +%Y%m%d%H%M%S`)
	(cd $*; ./TestEfficiency > time.`date +%Y%m%d%H%M%S`)
	cat $*/time* | grep -v True | sort -n | head -1 > $*/time.summary


# grep \; */time.summary | sed "s+/time.summary:+\; +g" | sort -n -t";" -k 2


# To load an approach, say EMGM, type run-EMGM
run-%:
	ghci -fglasgow-exts $(OPTS) -i$(patsubst run-%,%,$@)

