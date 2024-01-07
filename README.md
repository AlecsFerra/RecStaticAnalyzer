# REcSTaticAnalyzer

An abstract interpretation-based static analyzer for the REC programming language (which includes division) is detailed in the 9th chapter of the book “The Formal Semantics of Programming Languages” by G. Winskel, published by The MIT Press in 1993.

## Supported Analysis:
- Strictness
- Sign

## Usage

### Building the Project (Using Stack)

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

## Types of Analysis

### Strictness Analysis

The interpreter employs a two-point lattice (`Strict ≥ Lazy`), which is isomorphic to the Boolean lattice. This serves as an approximation.

A parameter is deemed strict if:
`pₖ = ⊥ ⇒ f p₁ ... pₙ ... pₖ  = ⊥  ∀ p₁ ... pₖ`.

Evaluating such parameters before invoking the function `f` enhances program interpretation efficiency.

The implementation is based on the description given by [Mycroft: "The theory and practice of transforming call-by-need into call-by-value"](https://doi.org/10.1007/3-540-09981-6_19) and [Clack, Peyton Jones: "Strictness analysis — a practical approach"](https://doi.org/10.1007/3-540-15975-4_28).

### Sign analysis
The interpreter leverages the extended sign domain

![image](https://user-images.githubusercontent.com/35380179/222907539-0b832f86-4f66-4710-9749-0d4a593f694d.png)

The implementation is based on the description given by [Antoine Miné - Static Inference of Numeric Invariants by Abstract Interpretation](https://doi.org/10.1561/2500000034).

## Adding new domains

The abstract interpreter in `src/Analysis` is designed to work with any lattice, and new abstract domains can be easily added by implementing the typeclass in `src/Data`.
