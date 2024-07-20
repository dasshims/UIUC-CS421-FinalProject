module Calctime where

import Data.List
import Data.Maybe
import Text.Regex
import Text.Printf


processData str = readFile str >>= formatAscii . processLines

table1 = processData "perf-results-6.8.2"
table2 = processData "perf-results-6.10.1"
table3 = processData "perf-results-6.8.3"
table4 = processData "perf-results-new"

formatAscii ls =
  do
  mapM_ (printf "%10s") ("":libraries)
  putStrLn ""
  mapM_ formatLine ls

processLines =
  reorderRows .
  aggregate (fst . fst) divideHand .
  aggregate fst (minimumBy (\x y -> compare (snd x) (snd y))) .
  catMaybes .
  map getSample .
  lines

tests = [("TestGEq","geq"),("TestSelectIntWTree","selectInt"),("TestRmWeights","rmWeights")]
reorderRows ls = catMaybes $ map (\ (n1,n2) -> fmap ((,) n2) $ lookup n1 ls) tests

libraries = ["LIGD","PolyP","SYB1_2","syb3","Spine","EMGM","RepLib","SmashA","Uniplate","multirec"]
divideHand ls = (test,map (`lookup` divided) libraries)
  where
    ([((test,_),timeHand)],ls') = partition (\x -> snd (fst x) == "Hand") ls
    divideTimes ((_test,lib),time) = (lib,time / timeHand)
    divided = map divideTimes ls'

formatLine (test,results) =
  do
  printf "%10s" test
  mapM_ pfield results
  putStrLn ""
  where
  pfield Nothing  = printf "%10s" "-"
  pfield (Just n) = printf "%10.2f" n

aggregate f aggr =
  map aggr . groupBy (\x y -> f x == f y) . sortBy (\x y -> compare (f x) (f y))

getSample s =
  fmap (\ (_,_,_,[test,lib,secs]) -> ((test,lib),read secs::Float)) $ 
  matchRegexAll (mkRegex outPattern) s
   
testStr = "out/TestSelectIntWTree.EMGM.out:375; 11"
outPattern = "^out/([A-Za-z]+)\\.([A-Za-z0-9_]+)\\.out:([0-9]+);"

