# origami-fold

[![Build Status](https://travis-ci.org/grumply/origami-fold.svg?branch=master)](https://travis-ci.org/grumply/origami-fold)

A type class of pure and recursive monadic origami-like lazy monoidal mapping folds.

Performs an analysis, monoidally, and transformation, top-down or bottom-up, in a single pass; traverses a structure to produce a monoidal result where the intermediate and final monoidal values can be used to transform the values of the structure. Evaluation demand is driven by the choice of monoid. Evaluation order is lazy in pure cases or driven by the laziness of the underlying recursive monad in monadic cases.

For instance, `foldo` can be used to calculate variance of a series and label each member of the series with its deviation in a single pass.

While this function can be seen to traverse both top-down and bottom-up, to be productive, one approach must be chosen at the use site; choosing both top-down and bottom-up simultaneously would result in ```<<loop>>```. There are derivative methods to prevent such misuse:

These methods (and their (M)onadFix implementations) are safe:
* foldll/M; fold from the start/top and combine monoidal values from left-to-right
* foldrl/M; fold from the end/bottom and combine monoidal values from left-to-right
* foldlr/M; fold from the start/top and combine monoidal values from right-to-left
* foldrr/M; fold from the end/bottom and combine monoidal values from right-to-left

The second (l|r) controls the evaluation of recursive substructures. For lists and sequences without substructure `foldll` and `foldlr` coincide and `foldrl` and `foldrr` coincide. This is not the case for trees where `foldll` is preorder traversal, `foldlr` is reverse preorder traversal, `foldrl` is postorder traversal and `foldrr` is reverse postorder traversal. `foldo` for trees is thus a single-algorithm generalization of pre- and post- order traversals where evaluation order is selected by the choice of monoid or the monoid and the recursive monadic context. 

### Example

As an example, imagine walking a structure and labeling each element with the length of the path from the start to that element plus the count of elements after the current element in the structure plus one for the current element divided by the total number of elements in the structure - a sort of relative weight.

```haskell
foldll (\ancestors@as descendants@ds total@t current@c -> (getSum (as <> ds <> Sum 1) / getSum t)) (\_ _ -> Sum 1)
```

For a tree, this can be seen as isolating the path to an element plus possible future paths and weighing that relative to the entire tree. This is a powerful lazy traversal operating in a single pass.

```
 1               ==>        1.0
 |                          |
 +- 2                       +- 0.3
 |  |                       |   |
 |  `- 3                    |   `- 0.3
 |                          |
 +- 4                       +- 0.2
 |                          |
 `- 5                       `- 0.7
    |                           |
    `- 6                        `- 0.7
       |                            |
       `- 7                         `- 0.7
          |                             |
          +- 8                          +- 0.5
          |                             |
          +- 9                          +- 0.5
          |                             |
          `- 10                         +- 0.5
```

Note that the above fold doesn't utilize the previous monoidal value or the current element in the production `(\_ _ -> Sum 1)`. Origami folds are capable of generating their monoidal values relative to historical monoidal values and the current element before transformation, this production just happens to not do so.
