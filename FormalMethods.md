# `Formal methods`

* Mathematically *prove* or *disprove* the correctness of a system's design and behaviour according to formal specifications 
* Shows both the *presence* and *absence* of specific errors
* For language design, software, hardware, algorithms

## Quickstart

Formal methods comprise the following 2 steps

1. ***Specification***
    * Describe the desired properties (and behaviors) of a system 
    * Written in formal languages 
        1. Tempora logic: specifies properties of systems that evolve over time
        2. Algebraic specification: defines abstract datatypes and their operations

2. ***Verification***
    1. **Model checking**
        * System is modelled as a finite state model
        * Exhaustively explores whether user-specified properties *(expressed in temporal logic)* hold in every possible model state
        * Suitable for hardware verification and concurrent systems  
            1. [*SPIN*](https://spinroot.com/spin/whatispin.html): widely-used model checker for verifying the correctness of distributed software models
            2. [*NuSMV*](https://nusmv.fbk.eu/): symbolic model checker supporting both BDD-based and SAT-based verification
        * Pros
            * fully automated
            * able to handle concurrent systems
            * produces counterexamples when a given property does NOT hold
        * Cons
            * suffers from State Space Explosion *(as a system grows, the number of states becomes unmanageably large)*  
            * limited to finite state systems  
    2. **Theorem proving**
        * System is described in a formal language
            1. [*Z Notation*](https://www.cs.umd.edu/~mvz/handouts/z-manual.pdf): used for describing the structure and behavior of software, particularly in safety-critical and high-assurance systems
            2. [*TLA+*](https://lamport.azurewebsites.net/tla/tla.html): used for reasoning about concurrent and distributed systems, enabling precise descriptions of software system behaviors
            3. [*Alloy*](https://alloytools.org/): lightweight modeling language for specifying complex structures and constraints in software systems
            4. [*VDM*](https://www.overturetool.org/method/): Vienna Development Method is a formal method that provides a set of modeling languages for specifying and verifying software systems in the early stages of design 
            5. [*VHDL*](https://www.copperpodip.com/post/vhdl-understanding-the-hardware-description-language): VHSIC Hardware Description Language describes the behavior and structure of electronic systems, commonly used in the design and simulation of digital circuits and hardware components
            6. [*Verilog*](https://www.chipverify.com/tutorials/verilog): hardware description language for modeling electronic systems at various levels of abstraction from gate level to system-level
            7. [*PVS*](https://pvs.csl.sri.com/): Prototype Verification System used for formal specification and verification of algorithm in areas like automated reasoning
            8. [*Coq*](https://coq.inria.fr/): formal language and proof assistant for developing and verifying mathematical proofs and algorithms, particularly in functional programming and theorem proving
            9. [*Isabelle*](https://isabelle.in.tum.de/): generic proof assistant used for formal verification of both software and hardware systems
    3. **Symbolic execution**
        * Program is executed with symbolic inputs instead of literal values
        * Explores multiple execution paths simultaneously 
            1. [*KLEE*](http://klee-se.org/releases/docs/v1.3.0/projects/): symbolic execution tool that automatically generates high-coverage tests 
        * Pros
            * capable of analyzing ALL possible execution paths in a program
            * finds bugs in code WITHOUT requiring a full formal specification
            * checks for errors like division by zero or buffer overflows
        * Cons
            * suffers from Path Explosion *(as program complexity grows, the number of paths quickly becomes unmanageably large)*  
            * limited to certain types of errors

## More on

* [Formal Methods for Software Specification and Analysis: An Overview](https://web.mit.edu/16.35/www/lecturenotes/FormalMethods.pdf)
* [Lecture 6: Introduction To Formal Methods](https://www.cs.ox.ac.uk/people/michael.wooldridge/teaching/soft-eng/lect06.pdf) by Mike Wooldridge
* [A Short Introduction to Formal Methods](https://software.imdea.org/~mcarro/Material/Formal_Methods/Formal_Methods_Intro/formal-methods_3.pdf) by Manuel Carro
* [Introduction to Formal Verification](https://ptolemy.berkeley.edu/projects/embedded/research/vis/doc/VisUser/vis_user/node4.html) by The Donald O Pederson Center for Electronic Systems Design
* [Formal Verification, Casually Explained](https://ahelwer.ca/post/2018-02-12-formal-verification/) by Andrew Helwer
* [A Gentle Introduction to Formal Verification](https://www.systemverilog.io/verification/gentle-introduction-to-formal-verification/) by sv:io
* [Introduction to Formal Verification](https://www.eeweb.com/introduction-to-formal-verification/) by EEWeb