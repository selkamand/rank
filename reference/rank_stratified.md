# Stratified hierarchical ranking across multiple variables

`rank_stratified()` computes a single, combined rank for each row of a
data frame using **stratified hierarchical ranking**. The first variable
is ranked globally; each subsequent variable is then ranked **within
strata defined by all previous variables**.

## Usage

``` r
rank_stratified(
  data,
  cols = NULL,
  sort_by = "frequency",
  desc = FALSE,
  ties.method = "average",
  na.last = TRUE,
  freq_tiebreak = "match_desc",
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame. Each selected column represents one level of the
  stratified hierarchy, in the order given by `cols`.

- cols:

  Optional column specification indicating which variables in `data` to
  use for ranking, and in what order. Can be:

  - `NULL` (default): use all columns of `data` in their existing order.

  - A character vector of column names.

  - An integer vector of column positions.

- sort_by:

  Character scalar or vector specifying how to rank each non-numeric
  column. Each element must be either `"alphabetical"` or `"frequency"`,
  matching the behaviour of
  [`smartrank()`](https://selkamand.github.io/rank/reference/smartrank.md).
  If a single value is supplied it is recycled for all columns. For
  numeric columns, `sort_by` is ignored and ranking is always based on
  numeric order.

- desc:

  Logical scalar or vector indicating whether to rank each column in
  descending order. If a single value is supplied it is recycled for all
  columns.

- ties.method:

  Passed to [`base::rank()`](https://rdrr.io/r/base/rank.html) when
  resolving ties at each level; must be one of `"average"`, `"first"`,
  `"last"`, `"random"`, `"max"`, or `"min"`. See
  [`base::rank()`](https://rdrr.io/r/base/rank.html) for details.

- na.last:

  Logical, controlling the treatment of missing values, as in
  [`base::rank()`](https://rdrr.io/r/base/rank.html). If `TRUE`, `NA`s
  are given the largest ranks; if `FALSE`, the smallest. Unlike
  [`base::rank()`](https://rdrr.io/r/base/rank.html) or
  [`smartrank()`](https://selkamand.github.io/rank/reference/smartrank.md),
  `na.last` cannot be set to `NA` in `rank_stratified()`, because
  dropping rows would change group membership and break stratified
  ranking.

- freq_tiebreak:

  Character scalar or vector controlling how alphabetical tie-breaking
  works when `sort_by = "frequency"` and the column is
  character/factor/logical. Each element must be one of:

  - `"match_desc"` (default): alphabetical tie-breaking follows `desc`
    for that column (ascending when `desc = FALSE`, descending when
    `desc = TRUE`).

  - `"asc"`: ties are always broken by ascending alphabetical order.

  - `"desc"`: ties are always broken by descending alphabetical order.

  If a single value is supplied, it is recycled for all columns.

- verbose:

  Logical; if `TRUE`, emit messages when `sort_by` is ignored (e.g. for
  numeric columns), mirroring the behaviour of
  [`smartrank()`](https://selkamand.github.io/rank/reference/smartrank.md).

## Value

A numeric vector of length `nrow(data)`, containing stratified ranks.
Smaller values indicate "earlier" rows in the stratified hierarchy.

## Details

This is useful when you want a "truly hierarchical" ordering where, for
example, rows are first grouped and ordered by the frequency of
`gender`, and then within each `gender` group, ordered by the frequency
of `pet` **within that gender**, rather than globally.

The result is a single rank vector that can be passed directly to
[`base::order()`](https://rdrr.io/r/base/order.html) to obtain a
stratified, multi-level ordering.

Stratified ranking proceeds level by level:

1.  The first selected column is ranked globally, using `sort_by[1]`
    (for non-numeric) and `desc[1]`.

2.  For the second column, ranks are computed **separately within each
    distinct combination of values of all previous columns**. Within
    each stratum, the second column is ranked using `sort_by[2]` /
    `desc[2]`.

3.  This process continues for each subsequent column: at level *k*,
    ranking is done within strata defined by columns 1, 2, ..., *k-1*.

This yields a single composite rank per row that reflects a "true"
hierarchical (i.e. stratified) ordering: earlier variables define
strata, and later variables are only compared **within** those strata
(for example, by within-stratum frequency).

## Examples

``` r
library(rank)

data <- data.frame(
  gender = c("male", "male", "male", "male", "female", "female", "male", "female"),
  pet    = c("cat",  "cat",  "magpie", "magpie", "giraffe", "cat", "giraffe", "cat")
)

# Stratified ranking: first by gender frequency, then within each gender
# by pet frequency *within that gender*
r <- rank_stratified(
  data,
  cols = c("gender", "pet"),
  sort_by = c("frequency", "frequency"),
  desc = TRUE
)

data[order(r), ]
#>   gender     pet
#> 3   male  magpie
#> 4   male  magpie
#> 1   male     cat
#> 2   male     cat
#> 7   male giraffe
#> 6 female     cat
#> 8 female     cat
#> 5 female giraffe
```
