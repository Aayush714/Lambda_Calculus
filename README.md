# Lambda_Calculus
A simple functional programming language with lambda calculus features, boolean and natural number constants, pairs, and type inference. The substitution function is used in evaluation, and type inference ensures terms are well-typed according to the rules defined.

# Simple Functional Language Interpreter

This project defines a simple functional programming language with lambda calculus features, booleans, natural numbers, pairs, and type inference. The code includes data types, substitution, evaluation, and type inference functions. This README will guide you on how to use the code and understand its structure.

## Table of Contents

1. [Getting Started](#getting-started)
2. [Data Types](#data-types)
3. [Substitution](#substitution)
4. [Evaluation](#evaluation)
5. [Type Inference](#type-inference)
6. [Usage](#usage)

## Getting Started

Ensure you have GHC (Glasgow Haskell Compiler) installed. Save the provided code into a file named `SimpleFunctionalLanguage.hs`.

### Compiling

To compile the Haskell file, use the following command in your terminal:

```bash
ghc SimpleFunctionalLanguage.hs
```

### Running

After compilation, you can run the program:

```bash
./SimpleFunctionalLanguage
```

## Data Types

### Term Data Type

The `Term` data type represents the constructs of the language:

```haskell
data Term = Var String
          | Lambda String Type Term
          | Tru
          | Fls
          | If Term Term Term
          | Zero
          | Succ Term
          | Pred Term
          | App Term Term
          | IsZero Term
          | TPair Term Term
          | FstPair Term
          | SndPair Term
          deriving (Eq, Show)
```

### Type Data Type

The `Type` data type represents the types in the language:

```haskell
data Type = TyFun Type Type
          | BoolType
          | NatType
          | ProdType Type Type
          deriving (Eq, Show)
```

## Substitution

The `subst` function performs substitution in a term, replacing occurrences of a variable with another term:

```haskell
subst :: String -> Term -> Term -> Term
```

### Example

```haskell
subst "x" (Succ Zero) (Var "x")
```

This will replace `x` with `Succ Zero` in the term `Var "x"`.

## Evaluation

### Single-Step Evaluation

The `eval1` function performs a single step of evaluation:

```haskell
eval1 :: Term -> Maybe Term
```

### Full Evaluation

The `eval` function repeatedly applies `eval1` until no more evaluation steps can be taken:

```haskell
eval :: Term -> Term
```

### Example

```haskell
eval (App (Lambda "x" NatType (Succ (Var "x"))) Zero)
```

This will evaluate the application of the lambda function to `Zero`.

## Type Inference

### Environment and Helper Functions

An environment (`Env`) maps variable names to their types:

```haskell
type Env = [(String, Maybe Type)]
```

The `findType` function infers the type of a term given an environment:

```haskell
findType :: Env -> Term -> Maybe Type
```

### Example

```haskell
findType [] (Lambda "x" NatType (Succ (Var "x")))
```

This will infer the type of the lambda function.

## Usage

To use this code, follow these steps:

1. **Define Terms:** Create terms using the `Term` data constructors.
2. **Substitute Terms:** Use the `subst` function to substitute variables in terms.
3. **Evaluate Terms:** Use the `eval` function to fully evaluate terms.
4. **Infer Types:** Use the `findType` function to infer the type of terms within an environment.

### Example Program

Here is an example program that creates a term, evaluates it, and infers its type:

```haskell
import SimpleFunctionalLanguage

main :: IO ()
main = do
    let term = App (Lambda "x" NatType (Succ (Var "x"))) Zero
    let evaluatedTerm = eval term
    let termType = findType [] term
    putStrLn $ "Original Term: " ++ show term
    putStrLn $ "Evaluated Term: " ++ show evaluatedTerm
    putStrLn $ "Type of Term: " ++ show termType
```

### Running the Example

Save the above program in a file named `Main.hs`, compile it with:

```bash
ghc Main.hs
```

Run the compiled program:

```bash
./Main
```

This will display the original term, the evaluated term, and its type.

## Conclusion

This project demonstrates a simple functional programming language with evaluation and type inference. Use the provided functions to create, evaluate, and type-check terms in this language. For more complex terms and additional functionality, extend the existing code as needed.
