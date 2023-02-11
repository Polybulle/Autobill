# Autobill: A generic resource analyser for functional languages 

**Note: This is alpha-quality software. A beta version is planned for early January 2023.**

Autobill is an experimental programming language for resource analysis. It based
on call-by-push-value lambda-calculus, with a polymorphic type system (*à la*
System F). The memory usage of Autobill programs can be automatically estimated
in three steps:
- Programs are automatically translated with explicit resource management;
- Explicit resources are typed to extract equations bounding their amounts;
- Those equations are solved using an off-the-shelf solver. 


## Installing, building, testing 

Autobill has been built and tested on macOS 12.6 and Ubuntu 22.04. You will need:
- a recent version of OCaml (4.13 and upwards confirmed to work)
- the `dune` build system
- the `menhir` parser generator

``` shell
#download opam and sources 
sudo apt-get install opam # for Ubuntu/Debian 
brew install opam # for MacOS 
git clone https://gitlab.lip6.fr/suzanneh/autobill.git
cd autobill
git checkout PSTL

#install dependencies
opam init
opam switch create autobill ocaml-base-compiler.4.14.1
opam install dune menhir dune-build-info

#Build
dune build
dune test
dune install 

#Install or run without installing
dune exec --context=autobill lcbpv <your options>  #for use without install within dune
```

## LCBPV Usage 

``` 
lcbpv [-o <outpath>] [-r] [-V] [-vpmisctl] [<inpath>]
```

By default, `autobill` expects a program on `stdin` and prints a desugared,
type-annotated program to `stdout`. 
- Use `autobill <inpath>` to read the input from `inpath` instead.
- Use `-o <outpath>` to print output to `outpath`. 
- Use `-r` to not run the simplification pass before typechecking.
- Use `-V` for a debug trace of the whole process (for developing `autobill` only).

Options `-vpisct` control the compilation pipeline. With no options, `autobill`
runs through the entire pipeline.
- Use `-v`, print the current version info and exit. 
- With `-p`, parse the program;
- With `-m`, convert to machine code;
- With `-i`, rename all identifiers with unique names;
- With `-s`, infer the indexed sort of all types;
- With `-c`, generate a typing constraint;
- With `-t`, solve the constraint and elaborate the types back in the source.
- With `-C`, print the remaining index constraint 
- With no options, run the commands after typing. 

## Introduction to Autobill
 
TODO
