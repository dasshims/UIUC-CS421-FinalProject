## Project repo for UIUC CS421: Programming languages and Compilers 


In my final project for CS421, I will review the paper "Comparing Libraries for Generic Programming in Haskell" by Alexey Rodriguez Yakushev, Johan Jeuring, Patrik Jansson, Alex Gerdes, Oleg Kiselyov, and Bruno C. d. S. Oliveira. This paper provides a suite to evaluate various libraries for generic programming, comparing their ease of use, expressiveness, performance, type safety, and interoperability. My project will include a detailed summary of the paper, an analysis of its findings, and a final report.

Paper Citation:
Rodriguez, Alexey & Jeuring, Johan & Jansson, Patrik & Gerdes, Alex & Kiselyov, Oleg & Oliveira, Bruno. (2008). Comparing Libraries for Generic Programming in Haskell. Haskell'08 - Proceedings of the ACM SIGPLAN 2008 Haskell Symposium. 44. 111-122. 10.1145/1411286.1411301.

Original Paper can be found [here](https://dl.acm.org/doi/abs/10.1145/1543134.1411301).


## Generic programming libraries comparison/benchmark
------------

The root directory contains a set of haskell programs that test generic
functions. Each test consists of a module written by the "end user" in
which he/she calls a type-indexed function on an existing data type
(usually Company). This Haskell module, which may be found at the root
directory, i.e. TestGEq.hs , imports a generic module. It is this generic
module that contains the generic function, already specialised for the
requested data type.

Such a generic module exists for each generic programming library, it can
be found in the library directory, i.e. LIGD/GEq.hs .  This module contains
the generic function itself, which is written by the power user. The
specialization of this generic function to, say, the Company data type is
written by the End User.

These generic modules use the framework provided by the generic library,
which is written by the Library Writer.

## Running the tests

Run the tests as follows:

```bash
cd generics/comparison
runghc test.hs --all
```

## Generating expected results

Example to generate expected for TestSelectSalary:
```bash
runghc test --expected LIGD TestSelectSalary
```

## Initial Setup
For Initial setup you might need to install few libraries in your system, it will depepnd how your system is already setup. Few common examples are - 
```bash
cabal install regex-compat
cabal install --lib regex-compat
```
