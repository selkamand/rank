
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rank

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/rank)](https://CRAN.R-project.org/package=rank)
[![R-CMD-check](https://github.com/selkamand/rank/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/selkamand/rank/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/selkamand/rank/branch/master/graph/badge.svg)](https://app.codecov.io/gh/selkamand/rank?branch=master)
<!-- badges: end -->

Rank provides a customizable alternative to the built-in `rank()`
function. The package offers the following features:

1.  **Frequency-based ranking of categorical variables**: choose whether
    to rank based on alphabetic order or element frequency.

2.  **Control over sorting order**: Use `desc=TRUE` to rank based on
    descending or ascending order.

## Installation

To install **rank** from CRAN run:

``` r
install.packages("rank")
```

You can install the development version of rank like so:

``` r
# install.packages('remotes')
remotes::install_github("selkamand/rank")
```

## Usage

``` r
library(rank)

fruits <- c("Apple", "Orange", "Apple", "Pear", "Orange")

## CATEGORICAL INPUT -----------------------

# rank alphabetically
smartrank(fruits)
#> [1] 1.5 3.5 1.5 5.0 3.5
```

``` r

# rank based on frequency
smartrank(fruits, sort_by = "frequency")
#> smartrank: Sorting a categorical variable by frequency: ignoring ties.method
#> [1] 2 3 2 1 3
```

``` r

# rank based on descending order of frequency
smartrank(fruits,sort_by = "frequency", desc = TRUE)
#> smartrank: Sorting a categorical variable by frequency: ignoring ties.method
#> [1] 1 2 1 3 2
```

``` r


## NUMERICAL INPUT -----------------------

# rank numerically
smartrank(c(1, 3, 2))
#> [1] 1 3 2
```

``` r

# rank numerically based on descending order
smartrank(c(1, 3, 2), desc = TRUE)
#> [1] 3 1 2
```

``` r

# always rank numerically, irrespective of sort_by
smartrank(c(1, 3, 2), sort_by = "frequency")
#> smartrank: Sorting a non-categorical variable. Ignoring `sort_by` and sorting numerically
#> [1] 1 3 2
```
