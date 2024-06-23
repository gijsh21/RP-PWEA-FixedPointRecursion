# Fixed-Point Value Recursion with Algebraic Effects and Handlers in Haskell
This repository hosts the code used in the research paper "Fixed-Point Value Recursion with Algebraic Effects and Handlers in Haskell" (2024) by Gijs van der Heide (BSc thesis in Computer Science & Engineering at Delft University of Technology). It is available digitally here: []

## Abstract
Algebraic effects and handlers are a new programming technique that allows for the definition of abstractions as interfaces, with handlers providing modular, concrete implementations of these interfaces. In this paper, we consider algebraic effects and handlers implemented in Haskell, and explore how they behave under fixed-point (value) recursion. We give different possible implementations of fixed-point combinators for effectful functions, and work out their evaluation processes. We find that these functions behave very predictably under normal fixed-point recursion, while value recursion seems to be a much harder problem. We discuss the difficulties of implementing value recursion, and several possibile solutions are explored, but the question of whether a fixed-point combinator with value recursion semantics can exist at all in the presence of algebraic effects remains unanswered. 