-- Script to test the generic benchmarks

import Prelude hiding (exp)
import System
import Data.List
import Text.Regex
import Control.Monad

libraries = ["Hand", "LIGD","Spine","EMGM","SYB1_2","RepLib","SmashA","PolyP","Uniplate","syb3"]

programs = [
            "TestGEq"                  -- Tests Separate compilation and multiple arguments
                                       -- using a datatype representable by all approaches: BinTree.
                                       -- Note that this test that universe extension to BinTree works
                                       -- for all aproaches.
           ,"TestSelectIntWTree"       -- Tests transformations without ad-hoc
           ,"TestSelectIntPerfect"     -- Test if nested datatypes are supported
           ,"TestSelectSalary"         -- Tests transformations and ad-hoc
           ,"TestGMapQ1","TestGMapQ2"
                                       -- These test higher order functions
           ,"TestRmWeights"
                                       -- Test constructor ad-hoc cases
           ,"TestUpdateSalary"         -- Tests paradise
           ,"TestCrushRight","TestGMap"-- These test abstraction over type constructors
           ,"TestGShow","TestGShowExt" -- Tests generic show, and generic show with an extension for lists
           ,"TestGEqCompany","TestGEqGRose", "TestGEqNGRose" ,"TestNested"
                                       -- Tests a bigger universe size
           ,"TestFullTree"             -- Tests generic producing
--         ,"TestGLookup" -- no more testing of generic abstractions
           ]

lhs = (++".lhs")
runghc = "runghc"
ghccommand = "ghc"
outdir = "out/"

-- commands --
-- the glasgow-exts option must be preceded by
-- this option for runghc to work
ghc_options = " -O2 -i. -fglasgow-exts -main-is eff -o a.out --make -fforce-recomp"
ghc  ls     = ghccommand ++ ghc_options ++ " " ++ unwords ls
diff ls     = "diff -u " ++ unwords ls

srcdir dir   = "-i" ++ dir
redir  xs ys = xs ++ " > " ++ ys
redirA xs ys = xs ++ " >> " ++ ys
redir2 xs ys = xs ++ " 2> " ++ ys
out name     = outdir ++ name ++".out" -- result
compilestats s lib = outdir ++ s ++ '.':lib ++ ".compilestats" -- result
exp name     = name ++".exp" -- expected
libit lib_   = (++ '.':lib_) -- lib extension

keepgoing str = do res <- system str
                   return ()

run str err cont = do
  res <- system str
  case res of
    ExitFailure code -> err code
    ExitSuccess      -> cont

getErrorReport :: [FilePath] -> String -> IO [String]
getErrorReport names program = do
  contents <- mapM readFile names
  return $ concatMap getexc
         $ concatMap lines contents
  where
  pattern_6_8 = ".*(" ++ lhs program ++ ": .*)"
  getexc s = case matchRegexAll (mkRegex pattern_6_8) s of
               Nothing           -> []
               Just (_,_,_,excs) -> excs

runtest :: (String -> String) -> String -> String -> IO ()
runtest out lib program = do
  putStr ("Testing "++program++"; "++lib++"; ")
  keepgoing ("rm a.out")
  run (ghc [srcdir lib,lhs program] 
       `redir2` compilestats program lib
      )
      (\_ -> do
         let
           generated = [compilestats program lib] -- GHC 6.8
         errors <- getErrorReport generated program
         putStrLn $ case errors of
                      []  -> " [compilation failed] see " ++ compilestats program lib
                      [l] -> " [execution failed]   : " ++ l
                      ls  -> " [execution failed] \n" ++ unlines (map ("  "++) errors)
      )
      $ do
  run ("./a.out"
       `redirA` out program
      )
      (\_ -> do
         let
           generated = [compilestats program lib] -- GHC 6.8
         errors <- getErrorReport generated program
         putStrLn $ case errors of
                      []  -> " [compilation failed] see " ++ compilestats program lib
                      [l] -> " [execution failed]   : " ++ l
                      ls  -> " [execution failed] \n" ++ unlines (map ("  "++) errors)
      )
      $ do
  putStrLn " [ok]"

runlib out progs lib = do
  mapM_ (runtest (out . libit lib) lib) progs

runsuite progs lib = do
  putStrLn ("Running tests on library "++lib)
  runlib out progs lib

genExpected progs lib = do
  putStrLn ("Generating expected files using "  ++ lib)
  mapM_ (genOneExpected lib) progs

genOneExpected lib program = do
    putStr ("Processing "++program++" ... ")
    run (ghc [srcdir lib,lhs program] `redir` exp program)
        (\_ -> putStrLn " compilation failed")
        (putStrLn "[ok]")
  

-- Generated expected files using LIGD
makeExpected = genExpected programs "LIGD"

orElse :: [a] -> [a] -> [a]
orElse list def = if null list then def else list

expected = "--expected"

options = [expected]

main = do
  args <- getArgs
  let
    -- Give this flag to generate .exp
    wantExpected = any (expected==) args
    driver | wantExpected = genExpected
           | otherwise    = runsuite

    -- Run tests on all libraries and tests
    allOpt = "--all"
    doAll = allOpt `elem` args

    -- Which libs and tests?
    -- default given in "libraries" and "programs"
    elem2 x xs = x `elem` xs && not doAll
    libs' = filter (`elem2` libraries) args `orElse` libraries
    progs = filter (`elem2` programs ) args `orElse` programs

    -- We can only use one lib when generating .exp
    libs | wantExpected = take 1 libs'
         | otherwise    = libs'

    -- Example to generate expected for TestSelectSalary:
    --
    --   runghc test --expected LIGD TestSelectSalary
    
    -- replace this by proper option handling lib. like opt
    rest_opts = (((args \\ options) \\ libraries) \\ programs) \\ [allOpt]

    prInstructions = putStrLn (unlines
                     ["usage: "
                     ,"  runghc test.hs [--all] [LIBRARIES] [TESTS]"
                     ,""
                     ,"    --all        Run all tests and libraries."
                     ,"    LIBRARIES    Is any of: " ++ concat (intersperse " " libraries)
                     ,"    TESTS        Is any of: " ++ concat (intersperse " " programs)
                     ])

  if null args then prInstructions else
    if null rest_opts then mapM_ (driver progs) libs
      else putStrLn ("* Unknown options : " ++ show rest_opts) >> prInstructions
