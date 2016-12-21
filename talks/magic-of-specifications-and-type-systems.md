---
title: Magic of Specifications and Type Systems
break: Specifications and
date: 2016-08-18
synopsis: My presentation for the EECS 4080 course at York University, including introduction of Unit-B Web.
run-in: I took
---

I took the Computer Science Project course, [EECS 4080][eecs4080], in
summer 2016. My main objective was learning Haskell and to gain some experience
with working on formal methods.

I had the privilege of working with Prof. Jonathan Ostroff and Simon Hudon,
mainly on two projects:

1. [Literate Unit-B][literate-unitb], the verifier for the Unit-B formal method,
   and
2. [Unit-B Web][unitb-web], a web interface using Literate Unit-B for doing
   predicate calculus proofs.

From the Literate Unit-B codebase, written in Haskell, I decoupled the logic
module and used it to build Unit-B Web, also written in Haskell, which supports
the <span class="latex">L<sup>a</sup>T<sub>e</sub>X</span> syntax of the Unit-B
logic, renders user input on the page, and calls the sequent prover of the logic
module, which uses the Z3 SMT solver to check the validity of user input.

Further, I did a major refactoring that separated the type checker of Literate
Unit-B from its parser. Among other things, this refactoring paves the way for
my next task of implementing subtyping, as volunteer work.

Here are the resources:

- [Slides][slides]
- [Poster][poster]
- [Source][src] for slides and poster

Unit-B's GitHub organization is [github.com/unitb](https://github.com/unitb).

[eecs4080]: https://wiki.eecs.yorku.ca/course_archive/2015-16/S/4080/
[literate-unitb]: https://github.com/unitb/literate-unitb
[unitb-web]: https://github.com/unitb/unitb-web
[slides]: https://static.aminb.org/eecs4080-slides.pdf
[poster]: https://static.aminb.org/eecs4080-poster.pdf
[src]: https://github.com/aminb/eecs4080

<style>
/* Styling of TeX and LaTeX. */

.latex, .latex sub {
  font-size: 1em;
}

.latex sub, .latex sup {
  text-transform: uppercase;
}

.latex sub {
  vertical-align: -0.25em;
  margin-left: -0.1667em;
  margin-right: -0.125em;
}

.latex sup {
  font-size: 0.85em;
  vertical-align: 0.15em;
  margin-left: -0.36em;
  margin-right: -0.15em;
}
</style>
