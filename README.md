# Fixed-Point (Value) Recursion with Algebraic Effects and Handlers in Haskell
This repository hosts the code used in the research paper "Fixed-Point (Value) Recursion with Algebraic Effects and Handlers in Haskell" (2024) by Gijs van der Heide (BSc thesis in Computer Science & Engineering at Delft University of Technology). It is available digitally here: []

## Abstract
Algebraic effects and handlers are a new programming technique that allows for the definition of abstractions as interfaces, with handlers providing modular, concrete implementations of these interfaces. In this paper, we consider algebraic effects and handlers implemented in Haskell, and explore how they behave under fixed-point (value) recursion. We give different possible implementations of fixed-point combinators for effectful functions, and work out their evaluation processes. We find that these functions behave very predictably under normal fixed-point recursion, while value recursion seems to be a much harder problem. We discuss the difficulties of implementing value recursion, and several possibile solutions are explored, but the question of whether a fixed-point combinator with value recursion semantics can exist at all in the presence of algebraic effects remains unanswered. 

## Repository overview
The code is contained in several different files, in the `main` folder. This is a short description of the contents of each file:

`Prelude1.hs` contains the necessary infrastructure for working with algebraic effects and handlers in Haskell (from http://casperbp.net/posts/2023-07-algebraic-effects/).

`EFix.hs` contains the implementations and some test functions for the operators for normal recursion with the free monad.

`EffectChoice.hs`, `EffectRandomGenerator.hs`, `EffectState.hs`, and `EffectStrOut.hs` contain the implementations of the respective effects.

`SimpleFactorial.hs` contains the example of the fix function and a simple factorial function, as well as implementations of the factorial function for the different signatures for fixing with the free monad.

`ValueRecursionAttempts.hs` contains the example of the function with an effect that reads a character from the input discussed in section 3, as well as some code to test the dummy handlers method discussed in section 3.

`CircuitsExample.hs` contains the motivating example of the circuit simulator discussed in section 4 of the paper.

`ChoiceFunction.hs` contains the function that branches based on a randomly generated boolean through a choice effect, and for which an analysis of the evaluation process was done in section 4.
