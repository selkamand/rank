# Bring specified values in a vector to the front

Reorders a vector so that any elements matching the values in `values`
appear first, in the order they appear in `values`. All remaining
elements are returned afterward, preserving their original order.

## Usage

``` r
reorder_by_priority(x, priority_values)
```

## Arguments

- x:

  A character or numeric vector to reorder.

- priority_values:

  A vector of “priority” values. Elements of `x` that match entries in
  `priority_values` are moved to the front in the order they appear in
  `priority_values`. Values not found in `x` are ignored.

## Value

A reordered vector with priority values first, followed by all remaining
elements in their original order.

## Examples

``` r
reorder_by_priority(c("A", "B", "C", "D", "E"), c("C", "A"))
#> [1] "C" "A" "B" "D" "E"
reorder_by_priority(1:6, c(4, 2, 7))
#> [1] 4 2 1 3 5 6
```
