# FTSE BASE


This repository provides basic building blocks for the FTSE project wrt. algorithms, formalisms, tools and transformations. 

---
* **Algorithms (mostly on sets or graph-like structures)**
  * *Reachability* (determines all reachable states within a given transition system)
  * *SetTheory* (construction of power sets, construction of cross products)
  * *Bisimulation* (extensible behavioural equivalence checks wrt. predefined notions of bisimulation equivalency, e.g. of type `markovian action labelled strong`, `markovian strong` or `strong` bisimulation.
  * *TopologicalOrderProcessing* (performing operations on DAGs)

* **formalism (language models, parsers, serialization and transformation infrastructure)**
  * arith (arithmetic expressions)
  * logical (logical expressions)
  * set (set expressions)
  * spa (stochastic process algebra)
  * timenet (stochastic petri net language)
  * tra (transition system language)
  * trace (event trace representation) 

* **transformations (abstract transformation interface and numerous transformation implementations)**
<!-- 
  * AbstrTransformer (abstract transformation interface)
  * Convert
  * LogicExpr2SatPaths
  * LogicExpr2SatPathsImpl
  * SATPATH_Generator
  * TRA2SPA
  * TRA2SPAImpl
  * TRA_Generator
  * TraElimination (vanishing states elimination of a transition system)
  * TraEmbeddedMarkovChain 
  * TraTransformer
  * TraUniformisation
-->

* **tools (wrappers, loggers or simple interactions/workflows)**
<!-- 
  * BddLogger
  * CASPA (binding to the command line tool CASPA as back-end)
  * CompositionLogger
  * DotLogger
  * Logger
  * PetriNetLogger 
  * Scripting (allows to build workflows using command line tools)
  * SPA_SPA_Eliminate (alternating composition and elimination of arising immediate transitions of CASPA Stochastic Process Algebra Models)

-->
--- 
## Prerequisites
Everything required for building should be specified in `build.sbt` using the [Simple Build Tool - SBT](https://www.scala-sbt.org/). It may also be a viable option to install Scala as well. 

## Building
To build and package the library 
```bash
sbt compile package
```

## Usage
Start e.g. java REPL
```bash
scala -cp target/scala-2.11/base_2.11-1.0.0.jar
```

And import stuff ... 
```scala
scala> import ftse.algorithms._
scala> ...
```
----
## Links: 
* [LARES website](http://lares.w3.rz.unibw-muenchen.de/about.html)
* [Design of Computer and Communication Systems Group @BUM](https://www.unibw.de/technische-informatik/mitarbeiter/professoren/siegle/forschung/entwurf-von-rechen-und-kommunikationssystemen)
* [PhD Thesis](https://athene-forschung.unibw.de/85049?query=A+Specification+Language+for+Reconfigurable+Dependable+System%2C+its+Formalization+and+Analysis+Environment&show_id=92070&id=85049)

