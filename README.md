# pipemimic

## Overview

This work of mine was done during my last semester in college since it was designed to be presented in my **Bachelor's Degree Thesis**. It is very important to state that my work was highly **motivated by [CheckTools](https://check.cs.princeton.edu/)**. I've read most of their papers, especially *Pipe Check* for many times for better understanding. Although "pipemimic"(a.k.a. this repo) was implemented in Scala, a considerable part of algorithms and data structures inside were **borrowed(re-implemented) from Coq source codes** in repo [daniellustig/pipecheck](https://github.com/daniellustig/pipecheck). Additionally, that's why I named it pipe "mimic".

Basically, I just added some features here and there to enable Pipe Check to parse [litmus tests for RVWMO](https://github.com/litmus-tests/litmus-tests-riscv)(RISC-V Weak Memory Ordering), including

- a parser for `.litmus` file (using ANTLR)
- data/ctrl/addr dependency checking in parsing phase
- `fence rw,rw` instruction support
- checking final state of memory is consistent with requirements stated in litmus test

Besides, a presentation on this work was given at RISC-V World Conference China 2021. You may check my [slides](./slides.pdf)(in Chinese) for more information.

In this repo you may find: 

- `./parser`: ANTLR source code
- `./pipemimic`: Scala source code
- `./scripts`: Python scripts used for converting csv files into pgfplots graphs
- `./profiling`: csv files generates by scala
- `./plots-data`: graphs (pgfplots format)

## Reference

Daniel Lustig, Michael Pellauer, and Margaret Martonosi. "PipeCheck: Specifying and Verifying Microarchitectural Enforcement of Memory Consistency Models", *47th International Symposium on Microarchitecture (MICRO)*, Cambridge UK, December 2014.