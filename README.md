# This project is to show what the primitive recursive function is, and how to optimize in Scheme.
`recbase.ss` provides z/s/p/comp/primitive-rec to design the primitive recursive function, and the function named `call` is to run the function. `recbase-old.ss` gives a very slow version, because it has no optimization.
At first, `call` converts the primitive recursive function design to the lambda-expr. Then, it takes several passes to optimize the lambda-expr. In the end, it runs the lambda-expr.
`call` avoids the symbols of the different action scopes using the same name, so that it will not use alpha converts in the optimization.
`functions-test.ss` gives some examples of normal natural numeral functions.
