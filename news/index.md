# Changelog

## rank (development version)

- Enhanced
  [`smartrank()`](https://selkamand.github.io/rank/reference/smartrank.md):
  - Added the `freq_tiebreak` argument with options `"match_desc"`,
    `"asc"`, and `"desc"` to control alphabetical tie-breaking when
    ranking character vectors by frequency.
- Added a new hierarchical ranking function:
  - [`rank_stratified()`](https://selkamand.github.io/rank/reference/rank_stratified.md)
    performs **true stratified hierarchical ranking** across multiple
    variables. Each variable is ranked within groups defined by all
    previous variables.
- Added two new functions for priority-based ranking:
  - [`rank_by_priority()`](https://selkamand.github.io/rank/reference/rank_by_priority.md)
    assigns highest ranks to user-specified values (in the order
    supplied), with all remaining values tied.
  - [`reorder_by_priority()`](https://selkamand.github.io/rank/reference/reorder_by_priority.md)
    reorders a vector so that priority values appear first, while
    preserving the original order of all non-priority elements.
- Added code of conduct and CONTRIBUTING.md

## rank 0.1.1

CRAN release: 2024-12-01

- Fixed documentation of ties.method paramater
  ([\#2](https://github.com/selkamand/rank/issues/2))

- Vignette added describing how to sort vectors or data.frames based on
  the results of `smartrank`
  ([\#3](https://github.com/selkamand/rank/issues/3),
  [\#4](https://github.com/selkamand/rank/issues/4))

- `ties.method` argument can now be used when ranking categorical
  variables by alphabetical order
  ([\#8](https://github.com/selkamand/rank/issues/8))

- Fixed an issue where smartrank ignored `desc=TRUE` when ranking
  categorical variables by alphabetical order
  ([\#5](https://github.com/selkamand/rank/issues/5))

## rank 0.1.0

CRAN release: 2024-07-09

- Initial CRAN submission.
