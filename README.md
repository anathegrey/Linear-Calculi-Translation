# Linear Calculi: a comparison approach

Evaluation and translation of two Linear Calculi: David Walker Calculus and Linear Haskell.

- To evaluate a term in the David Walker Calculus, use function ```runWO```, and to typecheck function ```runWT```.
- To evaluate a term in Linear Haskell, use function ```runLO```, and to typecheck function ```runLT```.
- To translate a term from David Walker Calculus to Linear Haskell, use function ```runWL``` and to translate from Linear Haskell to the David Walker Calculus, use function ```runLW```.
- To check if the heaps are saving the correct values, use ```runderefW``` or ```runderefL``` for the David Walker Calculus and Linear Haskell respectively.

There is also a file that contains a few examples, which can be imported to GHC as well.

The new type-variables, created by the algorithm, will use names starting with the letter "a" followed by a number, e.g., "a14".


Ana Jorge Almeida
