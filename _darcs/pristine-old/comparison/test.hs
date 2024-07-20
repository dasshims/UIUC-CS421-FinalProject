

-- Script to test the generic benchmarks

import Prelude hiding (exp)
import System

libraries = ["LIGD","SYB1_2","GM","GL","RepLib","Spine","Smash","NOW","PolyP"]

programs = ["TestGEq","TestGEqGRose","TestGEqTree","TestGShow","TestParadise"
           ,"TestFoldTree","TestReduce","TestGMap","TestNested"]
lhs = (++".lhs")
runghc = "runghc"

-- commands --
-- the glasgow-exts option must be preceded by
-- this option for runghc to work
ghc_options = " -i. -fglasgow-exts "
ghc ls = runghc ++ ghc_options ++ " " ++ unwords ls
diff ls = "diff -u " ++ unwords ls

srcdir dir = "-i" ++ dir
redir xs ys = xs ++ " > " ++ ys
out = (++".out") -- result
exp = (++".exp") -- expected
libit lib_ = (++ '.':lib_) -- lib extension

run str err cont = do
  res <- system str
  case res of
    ExitFailure code -> err code
    ExitSuccess -> cont

runtest :: (String -> String) -> String -> String -> IO ()
runtest out lib program = do
  putStr ("Testing "++program++" ... ")
  run (ghc [srcdir lib,lhs program] `redir` out program)
      (\_ -> putStrLn " [execution failed]")
      $ do
  run (diff [exp program,out program])
      (\_ -> putStrLn " [failed]")
      $ do
  putStrLn " [ok]"

runlib out lib = do
  mapM_ (runtest (out . libit lib) lib) programs

runsuite lib = do
  putStrLn ("Running tests on library "++lib)
  runlib out lib

genExpected lib = do
  putStrLn "Generating expected files"
  mapM_ (\ program -> do
    putStr ("Processing "++program++" ... ")
    run (ghc [srcdir lib,lhs program] `redir` exp program)
        (\_ -> putStrLn " compilation failed")
        (putStrLn "[ok]")
    )
    programs

-- Generated expected files using LIGD
makeExpected = genExpected "LIGD"

main = do
  args <- getArgs
  if length args == 0
    then mapM_ runsuite libraries
    else mapM_ runsuite args
