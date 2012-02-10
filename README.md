This example, based on chapter three of Richard Bird's Introduction to
Functional Programming using Haskell (2nd ed.) and section two of Erik
Meijer's banana paper, is intended to convey several key ideas:

- Definition of natural numbers as a very simple sublinear recursive
  algebraic data type. Think of Nat as structurally equivalent to List
  but without places to put items.

- Separation of concerns between traversal and processing: algebras
  over algebraic data types allow you to express common patterns of
  recursion as higher-order functions. The algebra for Nat is
  fashioned closely after the algebra of the linear List type.

- Type classes: how to make your own types more usable by making them
  instances of certain type classes.

- HUnit, including ways to reuse test cases through parameterization.
