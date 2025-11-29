test_that("smartrank() with defaults works identically to the built-in rank() function", {
  # Create a numeric vector to use as input
  x <- c(1, 3, 2, 5, 4)

  # Use both smartrank() and rank() on the vector, and compare the results
  expect_equal(smartrank(x, verbose = FALSE), rank(x))

  # Repeat with a character vector
  x <- c("a", "b", "b", "c", "c")
  expect_equal(smartrank(x, verbose = FALSE), rank(x))

  # Repeat with a numeric matrix
  x <- matrix(c(1, 3, 2, 5, 4, 6), ncol = 2)
  expect_equal(smartrank(x, verbose = FALSE), rank(x))

  # Repeat with a character matrix
  x <- matrix(c("a", "b", "b", "c", "c", "d"), ncol = 2)
  expect_equal(smartrank(x, verbose = FALSE), rank(x))
})


test_that("sorting by frequency works as expected", {
  x <- c("a", "b", "b", "c", "c")

  # Check the ranking when sort_by is set to "frequency"
  expect_equal(smartrank(x, sort_by = "frequency", verbose = FALSE), c(1, 2.5, 2.5, 4.5, 4.5))
})


test_that("desc flag works as expected for numeric vectors", {
  x <- c(1, 3, 2, 5, 4)

  # Check the ranking when desc is set to TRUE
  expect_equal(smartrank(x, desc = TRUE, verbose = FALSE), c(5, 3, 4, 1, 2))

  # Check the ranking when desc is set to FALSE
  expect_equal(smartrank(x, desc = FALSE, verbose = FALSE), c(1, 3, 2, 5, 4))
})

test_that("desc flag works as expected for character vectors", {
  x <- c("A", "B", "C", "D", "E", "E", NA_character_)

  # Check the ranking when desc is set to TRUE
  expect_equal(smartrank(x, sort_by = "alphabetical", desc = TRUE, verbose = FALSE, na.last = TRUE), c(6, 5, 4, 3, 1.5, 1.5, 7))

  # Check the ranking when desc is set to FALSE
  expect_equal(smartrank(x, sort_by = "alphabetical", desc = FALSE, verbose = FALSE, na.last = TRUE), c(1, 2, 3, 4, 5.5, 5.5, 7))
})


test_that("ties.method parameter works as expected", {
  x <- c(1, 2, 2, 2, 3)

  # Check the ranking when ties.method is set to "min"
  expect_equal(smartrank(x, ties.method = "min", verbose = FALSE), c(1, 2, 2, 2, 5))

  # Check the ranking when ties.method is set to "max"
  expect_equal(smartrank(x, ties.method = "max", verbose = FALSE), c(1, 4, 4, 4, 5))

  # Check the ranking when ties.method is set to "average"
  expect_equal(smartrank(x, ties.method = "average", verbose = FALSE), c(1, 3, 3, 3, 5))
})


test_that("'na.last' works as expected for numeric variables", {
  x <- c(1, NA, 2, NA, 3)

  # Check the ranking when na.last is set to TRUE
  expect_equal(smartrank(x, na.last = TRUE, verbose = FALSE), c(1, 4, 2, 5, 3))

  # Check the ranking when na.last is set to FALSE
  expect_equal(smartrank(x, na.last = FALSE, verbose = FALSE), c(3, 1, 4, 2, 5))

  # Check the ranking when na.last is set to TRUE & desc = TRUE
  expect_equal(smartrank(x, na.last = TRUE, desc=TRUE, verbose = FALSE), c(3, 4, 2, 5, 1))
})


test_that("'na.last' works as expected for categorical variables", {
  x <- c("a", NA, "b", NA, "c", "a", "b")

  # Check the ranking when na.last is set to TRUE and sort_by is set to "alphabetical"
  expect_equal(smartrank(x, sort_by = "alphabetical", na.last = TRUE, verbose = FALSE), rank(x, na.last = TRUE))

  # Check the ranking when na.last is set to FALSE and sort_by is set to "alphabetical"
  expect_equal(smartrank(x, sort_by = "alphabetical", na.last = FALSE, verbose = FALSE), rank(x, na.last = FALSE))

  # Check the ranking when na.last is set to TRUE & desc = TRUE
  expect_equal(smartrank(x, na.last = TRUE, desc=TRUE, verbose = FALSE), c(4.5, 6, 2.5, 7, 1, 4.5, 2.5))

  # Check the ranking when na.last is set to TRUE and sort_by is set to "frequency"
  expect_equal(smartrank(x, sort_by = "frequency", na.last = TRUE, verbose = FALSE), c(2.5, 6, 4.5, 7, 1, 2.5, 4.5))

  # Check the ranking when na.last is set to FALSE and sort_by is set to "frequency"
  expect_equal(smartrank(x, sort_by = "frequency", na.last = FALSE, verbose = FALSE), c(4.5, 1, 6.5, 2, 3, 4.5, 6.5))

  # NAs are dropped if na.last = NA
  expect_equal(smartrank(x, sort_by = "alphabetical", na.last = NA, verbose = FALSE), rank(x, na.last = NA))
  expect_equal(smartrank(x, sort_by = "frequency", na.last = NA, verbose = FALSE), c(2.5, 4.5, 1, 2.5, 4.5))


  # What about when input vec is all NAs
  x <- c(NA, NA, NA)
  expect_equal(smartrank(x, sort_by = "frequency", na.last = TRUE, verbose = FALSE), c(1, 2, 3))
  expect_equal(smartrank(x, sort_by = "frequency", na.last = FALSE, verbose = FALSE), c(1, 2, 3))
  expect_equal(smartrank(x, sort_by = "alphabetical", na.last = TRUE, verbose = FALSE), c(1, 2, 3))
  expect_equal(smartrank(x, sort_by = "alphabetical", na.last = NA, verbose = FALSE), rank(x, na.last = NA))
  expect_equal(smartrank(x, sort_by = "frequency", na.last = NA, verbose = FALSE), numeric(0))
})

# Test with all NAs
test_that("smartrank handles all NAs correctly", {
  x <- c(NA, NA, NA)
  expect_equal(smartrank(x), c(1, 2, 3))
  expect_equal(smartrank(x, na.last = NA), numeric(0))
})

# Test with NAs in different positions
test_that("smartrank handles NAs in different positions", {
  x <- c(NA, "b", "a", NA, "c")
  expect_equal(smartrank(x), c(4, 2, 1, 5, 3))
  expect_equal(smartrank(x, na.last = FALSE), c(1, 4, 3, 2, 5))
  expect_equal(smartrank(x, na.last = NA), c(2, 1, 3))
})

# Test with na.last = FALSE
test_that("smartrank handles na.last = FALSE correctly", {
  x <- c("a", NA, "b")
  expect_equal(smartrank(x, na.last = FALSE), c(2, 1, 3))
})

test_that("smartrank function throws appropriate errors when incorrect parameter types are supplied", {
  x <- c(1, 2, 3)

  expect_error(smartrank(x, sort_by = "abc"), "sort_by must be one of 'alphabetical' or 'frequency'")
  expect_error(smartrank(x, ties.method = "abc"), 'ties.method should be one of: "average", "first", "last", "random", "max", or "min"')
  expect_error(smartrank(x, na.last = "abc"), "na.last must be TRUE/FALSE")
  expect_error(smartrank(x, verbose = NA), "verbose must be TRUE/FALSE")
})

test_that("verbose flag works as expected", {
  # Test numeric input
  x <- c(1, 2, 3)
  expect_message(smartrank(x, sort_by = "frequency", verbose = TRUE), "smartrank: Sorting a non-categorical variable. Ignoring `sort_by` and sorting numerically\n")
  expect_message(smartrank(x, sort_by = "frequency", verbose = FALSE), NA)
})

test_that("categorical sorting by frequency works with ties.method", {

  # Test categorical input
  x <- c("A", "C", "B", NA, "C", NA)
  expect_equal(smartrank(x, sort_by = "frequency", ties.method = "average", na.last = TRUE, desc = FALSE), c(1, 3.5, 2, 5, 3.5, 6))
  # expect_equal(smartrank(x, sort_by = "frequency", ties.method = "average", na.last = FALSE, desc = FALSE), c(3, 6.5, 5, 4, 3.5, 2))
})

# Test with ties.method = "first"
test_that("smartrank handles ties.method = 'first'", {
  x <- c("b", "a", "b", "a")
  expect_equal(smartrank(x, ties.method = "first"), c(3, 1, 4, 2))
})

# Test with ties.method = "random"
test_that("smartrank handles ties.method = 'random'", {
  x <- c("b", "a", "b", "a")
  ranking <- smartrank(x, ties.method = "random")
  expect_true(all(ranking %in% 1:4))
  expect_true(length(unique(ranking)) == 4)
})

# Test with ties.method = "max"
test_that("smartrank handles ties.method = 'max'", {
  x <- c("b", "a", "b", "a")
  expect_equal(smartrank(x, ties.method = "max"), c(4, 2, 4, 2))
  expect_equal(smartrank(x, ties.method = "max"), c(4, 2, 4, 2))
})

test_that("smartrank function throws an error when sort_by is not a character vector of length 1", {
  expect_error(smartrank(1:3, sort_by = NULL), "sort_by must be one of 'alphabetical' or 'frequency'")
  expect_error(smartrank(1:3, sort_by = 1), "sort_by must be one of 'alphabetical' or 'frequency'")
  expect_error(smartrank(1:3, sort_by = TRUE), "sort_by must be one of 'alphabetical' or 'frequency'")
})

test_that("smartrank correctly ranks when desc == TRUE and sort_by == 'frequency'", {
  x <- c("apple", "banana", "apple", "cherry", "banana", "banana")

  result <- smartrank(x, sort_by = "frequency", desc = TRUE, verbose = FALSE)

  expect_equal(result, c(4.5, 2, 4.5, 6, 2, 2))
})

test_that("desc flag flips secondary and tertiary sort when frequencies are equal", {
  x <- c("A", "A", "B", "B", "C", "C")

  # When frequencies are the same, ties are broken alphabetically then by original index when desc = FALSE
  result_false <- smartrank(x, sort_by = "frequency", desc = FALSE, verbose = FALSE)

  # When desc = TRUE, we should invert the alphabetical and index order for ties
  result_true <- smartrank(x, sort_by = "frequency", desc = TRUE, verbose = FALSE)

  # The results should differ
  expect_false(identical(result_false, result_true),
               info = "Results should differ when desc is flipped if frequencies are tied")

  # Check the ordering behavior:
  # For desc = FALSE, after ranking, sorting by rank should yield elements in ascending alphabetical order (A, B, C)
  expect_equal(unique(x[order(result_false)]), c("A", "B", "C"))

  # For desc = TRUE, after updating the code to invert tie-breaks, sorting by rank should yield elements in descending alphabetical order (C, B, A)
  expect_equal(unique(x[order(result_true)]), c("C", "B", "A"))
})


test_that("smartrank function throws an error when na.last or verbose are not logical values", {
  x <- c(1, 2, 3)

  expect_error(smartrank(x, na.last = NULL), "na.last must be TRUE/FALSE")
  expect_error(smartrank(x, na.last = 1), "na.last must be TRUE/FALSE")

  expect_error(smartrank(x, verbose = NULL), "verbose must be TRUE/FALSE")
  expect_error(smartrank(x, verbose = 1), "verbose must be TRUE/FALSE")
})

test_that("smartrank works with logical vectors", {
  x <- c(TRUE, FALSE, TRUE, FALSE, NA)

  # When sort_by == "alphabetical", desc == FALSE
  expect_equal(smartrank(x, sort_by = "alphabetical", desc = FALSE, verbose = FALSE), rank(x))

  # When sort_by == "alphabetical", desc == TRUE
  expect_equal(smartrank(x, sort_by = "alphabetical", desc = TRUE, verbose = FALSE), rank(!x, na.last = TRUE, ties.method = "average"))

  # When sort_by == "frequency", desc == FALSE
  # Frequencies: TRUE (2), FALSE (2)
  # Since frequencies are equal, alphabetical order is used: FALSE, TRUE
  expected <- c(3.5, 1.5, 3.5, 1.5, 5)
  result <- smartrank(x, sort_by = "frequency", desc = FALSE, verbose = FALSE)
  expect_equal(result, expected)

  # When sort_by == "frequency", desc == TRUE
  expected <- c(1.5, 3.5, 1.5, 3.5, 5)
  result <- smartrank(x, sort_by = "frequency", desc = TRUE, verbose = FALSE)
  expect_equal(result, expected)

})

test_that("smartrank works with factor vectors", {
  x <- factor(c("a", "c", "b", "a", "b", "c", NA))

  # When sort_by == "alphabetical", desc == FALSE
  expect_equal(smartrank(x, sort_by = "alphabetical", desc = FALSE, verbose = FALSE), rank(x))

  # When sort_by == "alphabetical", desc == TRUE
  expected <- c(5.5, 1.5, 3.5, 5.5, 3.5, 1.5, 7)
  expect_equal(smartrank(x, sort_by = "alphabetical", desc = TRUE, verbose = FALSE, na.last = TRUE), expected)

  # When sort_by == "frequency", desc == FALSE
  # Frequencies: "a" (2), "b" (2), "c" (2)
  # Since frequencies are equal, alphabetical order is used: "a", "b", "c"
  result <- smartrank(x, sort_by = "frequency", desc = FALSE, verbose = FALSE)
  expect_equal(result, c(1.5, 5.5, 3.5, 1.5, 3.5, 5.5, 7))
})


test_that("freq_tiebreak = 'match_desc' makes tie-breaking follow desc", {
  x <- c("a", "a", "b", "b")

  # Default behaviour: freq_tiebreak = "match_desc"
  r_false <- smartrank(x, sort_by = "frequency", desc = FALSE, verbose = FALSE)
  r_true  <- smartrank(x, sort_by = "frequency", desc = TRUE,  verbose = FALSE)

  # When desc = FALSE: alphabetical ascending for ties
  expect_equal(unique(x[order(r_false)]), c("a", "b"))

  # When desc = TRUE: alphabetical descending for ties
  expect_equal(unique(x[order(r_true)]),  c("b", "a"))
})

test_that("freq_tiebreak = 'asc' keeps alphabetical tie-breaking ascending regardless of desc", {
  x <- c("a", "a", "b", "b")

  r_false <- smartrank(x, sort_by = "frequency", desc = FALSE,
                       freq_tiebreak = "asc", verbose = FALSE)
  r_true  <- smartrank(x, sort_by = "frequency", desc = TRUE,
                       freq_tiebreak = "asc", verbose = FALSE)

  # In both cases, ties should break in ascending alphabetical order
  expect_equal(unique(x[order(r_false)]), c("a", "b"))
  expect_equal(unique(x[order(r_true)]),  c("a", "b"))
})

test_that("freq_tiebreak = 'desc' keeps alphabetical tie-breaking descending regardless of desc", {
  x <- c("a", "a", "b", "b")

  r_false <- smartrank(x, sort_by = "frequency", desc = FALSE,
                       freq_tiebreak = "desc", verbose = FALSE)
  r_true  <- smartrank(x, sort_by = "frequency", desc = TRUE,
                       freq_tiebreak = "desc", verbose = FALSE)

  # In both cases, ties should break in descending alphabetical order
  expect_equal(unique(x[order(r_false)]), c("b", "a"))
  expect_equal(unique(x[order(r_true)]),  c("b", "a"))
})

test_that("freq_tiebreak must be one of 'match_desc', 'asc', or 'desc'", {
  x <- c("a", "b", "b", "a")

  expect_error(
    smartrank(x, sort_by = "frequency", freq_tiebreak = "foo", verbose = FALSE),
    "match_desc"
  )
})

test_that("rank_stratified uses all columns when cols = NULL (default)", {
  data <- data.frame(
    g = c("A", "A", "B", "B"),
    x = c("z", "a", "z", "a")
  )

  # Explicit cols = NULL should match implicit default
  r_default <- rank_stratified(
    data,
    cols    = NULL,
    sort_by = c("alphabetical", "alphabetical"),
    desc    = c(FALSE, FALSE)
  )

  r_explicit <- rank_stratified(
    data,
    cols    = c("g", "x"),
    sort_by = c("alphabetical", "alphabetical"),
    desc    = c(FALSE, FALSE)
  )

  expect_equal(r_default, r_explicit)
})

test_that("rank_stratified respects column order specified in cols", {
  data <- data.frame(
    g = c("A", "A", "B", "B"),
    x = c("z", "a", "z", "a")
  )

  # Case 1: group first, then value
  r_gx <- rank_stratified(
    data,
    cols    = c("g", "x"),
    sort_by = c("alphabetical", "alphabetical"),
    desc    = c(FALSE, FALSE)
  )
  ord_gx <- order(r_gx)

  # All A before all B; within each, a then z
  expect_equal(data$g[ord_gx], c("A", "A", "B", "B"))
  expect_equal(
    data$x[ord_gx],
    c("a", "z", "a", "z")
  )

  # Case 2: value first, then group
  r_xg <- rank_stratified(
    data,
    cols    = c("x", "g"),
    sort_by = c("alphabetical", "alphabetical"),
    desc    = c(FALSE, FALSE)
  )
  ord_xg <- order(r_xg)

  # All rows with 'a' come before all rows with 'z'
  expect_equal(data$x[ord_xg], c("a", "a", "z", "z"))
})

test_that("rank_stratified accepts numeric column positions in cols", {
  data <- data.frame(
    g = c("A", "A", "B", "B"),
    x = c("z", "a", "z", "a")
  )

  r_names <- rank_stratified(
    data,
    cols    = c("g", "x"),
    sort_by = c("alphabetical", "alphabetical"),
    desc    = c(FALSE, FALSE)
  )

  r_pos <- rank_stratified(
    data,
    cols    = c(1L, 2L),
    sort_by = c("alphabetical", "alphabetical"),
    desc    = c(FALSE, FALSE)
  )

  expect_equal(r_names, r_pos)
})

test_that("rank_stratified errors when cols contains unknown column names", {
  data <- data.frame(
    g = c("A", "B"),
    x = c("a", "b")
  )

  expect_error(
    rank_stratified(data, cols = c("g", "not_a_col")),
    regexp = "All `cols` must be present in `data`"
  )
})

test_that("rank_stratified errors when cols is numeric but out of bounds", {
  data <- data.frame(
    g = c("A", "B"),
    x = c("a", "b")
  )

  expect_error(
    rank_stratified(data, cols = c(1L, 3L)),
    regexp = "Numeric `cols` must be valid column positions in `data`"
  )
})

test_that("rank_stratified errors when cols is zero-length", {
  data <- data.frame(
    g = c("A", "B"),
    x = c("a", "b")
  )

  expect_error(
    rank_stratified(data, cols = character(0)),
    regexp = "`cols` must contain at least one column name or position"
  )

  expect_error(
    rank_stratified(data, cols = integer(0)),
    regexp = "`cols` must contain at least one column name or position"
  )
})

test_that("rank_stratified errors when cols is of unsupported type", {
  data <- data.frame(
    g = c("A", "B"),
    x = c("a", "b")
  )

  expect_error(
    rank_stratified(data, cols = list("g")),
    regexp = "`cols` must be NULL, a character vector, or an integer vector"
  )
})


