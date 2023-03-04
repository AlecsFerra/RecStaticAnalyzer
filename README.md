# REcSTaticAnalyzer

An abstract interpretation based static analyzer for the REC programming language described in the 9th chapter of the book “The formal semantics of programming languages” by G. Winskel, The MIT Press, 1993..

Supported Analysis:
- Strictness
- Sign

## Usage
### Building the project (using stack)

```
stack build
```

Then add the `stan-exe` binary in your path.

### Rinning a rec program

Run a program:
```
stan-exe path/to/src --type
```
Where `type` is the name of the analysis to perform

## Types of analysis
### Stricntess analysis
The interpreter leverages a 2 point lattice (`Strict ≥ Lazy`) (isomorphic to the boolean lattice) obviously is an approximation.

A parameter is said strict when we have that `pₖ = ⊥ ⇒ f p₁ ... pₙ ... pₖ  = ⊥  ∀ p₁ ... pₖ`.
When a parameter is strict is safe to perform his evaluation before the call to the function f thus making the interpretation of the program more efficient.

The implementation is based on the description given by [Mycroft: "The theory and practice of transforming call-by-need into call-by-value"](https://doi.org/10.1007/3-540-09981-6_19) and [Clack, Peyton Jones: "Strictness analysis — a practical approach"](https://doi.org/10.1007/3-540-15975-4_28).

### Sign analysis
The interpreter leverages the extended sign domain

![image](https://user-images.githubusercontent.com/35380179/222896464-2d54d70d-e7d9-45e6-b76a-d77753f87642.png)

The implementation is based on the description given by [Antoine Miné - Static Inference of Numeric Invariants by Abstract Interpretation](https://doi.org/10.1561/2500000034).

## Adding new domains

The abstract interpreter in `src/Analysis` is designed to work with any lattice, and new abstract domains can be easily added by implementing the typeclass in `src/Data`.
