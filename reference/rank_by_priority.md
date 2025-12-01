# Rank a character vector based on supplied priority values

Rank a character vector based on supplied priority values

## Usage

``` r
rank_by_priority(x, priority_values, ties.method = "average")
```

## Arguments

- x:

  A character vector.

- priority_values:

  A character vector descibing "priority" values. Elements of `x`
  matching `priority_values` will be ranked based on their order of
  appearance in `priority_values`

- ties.method:

  a character string specifying how ties are treated, see ‘Details’; can
  be abbreviated.

## Value

A vector of ranks describing `x` such that `x[order(ranks)]` will move
`priority_values` to the front of the vector

## Examples

``` r
x <- c("A", "B", "C", "D", "E")
rank_by_priority(x, c("C", "A"))
#> [1] 2 4 1 4 4
#> "2" "4" "1" "4" "4"

rank_by_priority(1:6, c(4, 2, 7))
#> [1] 4.5 2.0 4.5 1.0 4.5 4.5
#>  4 2 1 3 5 6
```
