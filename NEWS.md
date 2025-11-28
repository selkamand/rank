# rank (development version)

* Added two new functions for priority-based ranking:
  - `rank_by_priority()` assigns highest ranks to user-specified values (in the
    order supplied), with all remaining values tied.
  - `reorder_by_priority()` reorders a vector so that priority values appear
    first, while preserving the original order of all non-priority elements.

* Added code of conduct

# rank 0.1.1

* Fixed documentation of ties.method paramater (#2)

* Vignette added describing how to sort vectors or data.frames based on the 
  results of `smartrank` (#3, #4)

* `ties.method` argument can now be used when ranking categorical variables by 
  alphabetical order (#8)

* Fixed an issue where smartrank ignored `desc=TRUE` when ranking categorical 
  variables by alphabetical order (#5)

# rank 0.1.0

* Initial CRAN submission.


