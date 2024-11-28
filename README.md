
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

# rank based on frequency
smartrank(fruits, sort_by = "frequency")
#> smartrank: Sorting a categorical variable by frequency: ignoring ties.method
#> [1] 2 3 2 1 3

# rank based on descending order of frequency
smartrank(fruits,sort_by = "frequency", desc = TRUE)
#> smartrank: Sorting a categorical variable by frequency: ignoring ties.method
#> [1] 1 2 1 3 2


## NUMERICAL INPUT -----------------------

# rank numerically
smartrank(c(1, 3, 2))
#> [1] 1 3 2

# rank numerically based on descending order
smartrank(c(1, 3, 2), desc = TRUE)
#> [1] 3 1 2

# always rank numerically, irrespective of sort_by
smartrank(c(1, 3, 2), sort_by = "frequency")
#> smartrank: Sorting a non-categorical variable. Ignoring `sort_by` and sorting numerically
#> [1] 1 3 2
```

### Sorting By Rank

We can use `order` to sort vectors based on their ranks. For example, we
can sort the `fruits` vector based on the frequency of each element.

``` r
fruits <- c("Apple", "Orange", "Apple", "Pear", "Orange")
ranks <- smartrank(fruits, sort_by = "frequency")
#> smartrank: Sorting a categorical variable by frequency: ignoring ties.method
fruits[order(ranks)]
#> [1] "Pear"   "Apple"  "Apple"  "Orange" "Orange"
```

### Working with data-frames

When working with data.frames, we not only wa

#### BaseR

For example, imagine we want to sort the following dataframe based on
frequency of fruits, but break any ties based on the alphabetical order
of the picker.

``` r
data = data.frame(
    fruits = c("Apple", "Orange", "Apple", "Pear", "Orange"),
    picker = c("Elizabeth", "Damian",  "Bob", "Cameron", "Alice")
)

# Rank fruits so the most frequently picked fruits will come first
fruit_ranks <- smartrank(data$fruits, sort_by = "frequency", desc=TRUE) 
#> smartrank: Sorting a categorical variable by frequency: ignoring ties.method

# Rank pickers in alphabetical order
picker_ranks <- smartrank(data$picker, sort_by = "alphabetical", desc=FALSE) 

# Sort dataframe by the fruit_ranks, then the picker_ranks (heirarchical sot)
data[order(fruit_ranks, picker_ranks),]
#>   fruits    picker
#> 3  Apple       Bob
#> 1  Apple Elizabeth
#> 5 Orange     Alice
#> 2 Orange    Damian
#> 4   Pear   Cameron
```

#### Tidyverse Integration

An equivalent way to hierarchically sort data.frames is to us
`smartrank` in the tidyverse `arrange()` function

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

# Hierarchical Sort
arrange(
  data, 
  smartrank(fruits, "frequency", desc = TRUE), 
  smartrank(picker, "alphabetical", desc = FALSE)
)
#> smartrank: Sorting a categorical variable by frequency: ignoring ties.method
#>   fruits    picker
#> 1  Apple       Bob
#> 2  Apple Elizabeth
#> 3 Orange     Alice
#> 4 Orange    Damian
#> 5   Pear   Cameron
```
