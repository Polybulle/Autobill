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
#install
sudo apt-get install ocaml dune menhir #for Ubuntu
brew install ocaml dune menhir  #for macOS
git clone https://gitlab.lip6.fr/suzanneh/autobill.git
cd autobill

#Build
dune build

#Test
dune test 

#Install or run without installing
dune install  #for installation
dune exec -- autobill <your options>  #for use without install within dune
```

## Usage 

``` 
autobill [-o <outpath>] [-r] [-V] [-vpisct] [<inpath>]
```

By default, `autobill` expects a program on `stdin` and prints a desugared,
type-annotated program to `stdout`. 
- Use `autobill <inpath>` to read the input from `inpath` instead.
- Use `-v`, print the current version info and exit. 
- Use `-o <outpath>` to print output to `outpath`. 
- Use `-r` to not run the simplification pass before typechecking.
- Use `-V` for a debug trace of the whole process (for developing `autobill` only).

Options `-pisct` control the compilation pipeline. With no options, `autobill`
runs through the entire pipeline.
- With `-p`, parse and pretty-print the program;
- With `-i`, rename all identifiers with unique names;
- With `-s`, infer the indexed sort of all types;
- With `-c`, generate a typing constraint;
- With `-t`, solve the constraint and elaborate the types back in the source.
- With no options, run the commands after typing. 

## Introduction to Autobill
 
TODO
