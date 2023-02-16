# Random notes written while working on this project

- function f is strict in (one of) its parameter if it preserves nontermination, i.e. f undefined = undefined

- it’s equivalent to say that if f is strict, then (either f = const undefined or) it evaluates its argument on every possible execution path

- How can we extend this to our intuitive notion of strictness in a variable that occurs free in some expression? We can just capture that free variable with a lambda and apply our original definition. So, when we talk about the strictness of n as it demanded in twice (\x -> x + n) m, we are actually talking about strictness properties of the function \n -> twice (\x -> x + n) m
