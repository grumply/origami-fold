# origami-fold

A type class representing middle-out origami-style monoidal mapping folds.

Performs an analysis, monoidally, and transformation, top-down or bottom-up, in a single pass; traverses a structure to produce a monoidal result where the intermediate and final monoidal values can be used to transform the values of the structure. 

For instance, this function can be used to calculate variance of a series and label each member of the series with its deviation in a single pass.

While this function can be seen to traverse both top-down and bottom-up, to be productive, one approach must be chosen at the use site; choosing both top-down and bottom-up simultaneously would result in <<loop>>. There are derivative methods to prevent misuse:

* foldsl; fold from the (s)tart/top and combine monoidal values from (l)eft-to-right
* foldel; fold from the (e)nd/bottom and combine monoidal values from (l)eft-to-right
* foldsr; fold from the (s)tart/top and combine monoidal values from (r)ight-to-left
* folder; fold from the (e)nd/bottom and combine monoidal values from (r)ight-to-left
