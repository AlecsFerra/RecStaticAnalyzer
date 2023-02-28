# REcSTrictnessAnalyzer

An abstract interpretation based strictness analyzer for the REC programming language described in the 9th chapter of the book “The formal semantics of programming languages” by G. Winskel, The MIT Press, 1993..

The interpreter leverages a 2 point lattice (`Strict ≥ Lazy`) to aproximate the strictness of the paremter of the functions.

A parameter is said strict when we have that `pₖ = ⊥ ⇒ f p₁ ... pₙ ... pₖ  = ⊥  ∀ p₁ ... pₖ`.
When a parameter is strict is safe to perform his evaluation before the call to the function f thus making the interpretation of the program more efficient.

The implementation is based on the description given by [Mycroft: "The theory and practice of transforming call-by-need into call-by-value"](https://doi.org/10.1007/3-540-09981-6_19) and [Clack, Peyton Jones: "Strictness analysis — a practical approach"](https://doi.org/10.1007/3-540-15975-4_28).

## Adding new domains

The abstract interpreter in `src/Analysis` is designed to work with any lattice, and new abstract domains can be easily added by implementing the typeclass in `src/Data`.
