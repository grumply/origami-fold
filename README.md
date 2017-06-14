# origami-fold

A type class representing middle-out origami-style monoidal mapping folds.

Performs an analysis, monoidally, and transformation, top-down or bottom-up, in a single pass; traverses a structure to produce a monoidal result where the intermediate and final monoidal values can be used to transform the values of the structure. 

For instance, this function can be used to calculate variance of a series and label each member of the series with its deviation in a single pass.

While this function can be seen to traverse both top-down and bottom-up, to be productive, one approach must be chosen at the use site; choosing both top-down and bottom-up simultaneously would result in <<loop>>. There are derivative methods to prevent misuse:

These methods are safe:
* foldsl; fold from the (s)tart/top and combine monoidal values from (l)eft-to-right
* foldel; fold from the (e)nd/bottom and combine monoidal values from (l)eft-to-right
* foldsr; fold from the (s)tart/top and combine monoidal values from (r)ight-to-left
* folder; fold from the (e)nd/bottom and combine monoidal values from (r)ight-to-left

### Example

As an example, to tag each element in a structure with the size of the path to that element plus the size of the substructure below it relative to the total number of elements in the structure.

```haskell
foldsl (\ancestors@as descendants@ds total@t element@e -> (getSum (as <> cs <> Sum 1) / getSum t,e)) (\_ _ -> Sum 1)
```

For a tree, this can be seen as isolating the path to an element plus possible future paths and weighing that relative to the entire tree. This is a powerful traversal that operates in a single pass.

```
 1               ==>        (1.0,1)
 |                          |
 +- 2                       +- (0.3,2)
 |  |                       |   |
 |  `- 3                    |   `- (0.3,3)
 |                          |
 +- 4                       +- (0.2,4)
 |                          |
 `- 5                       `- (0.7,5)
    |                           |
    `- 6                        `- (0.7,6)
       |                            |
       `- 7                         `- (0.7,7)
          |                             |
          +- 8                          +- (0.5,8)
          |                             |
          +- 9                          +- (0.5,9)
          |                             |
          `- 10                         +- (0.5,10)
```
