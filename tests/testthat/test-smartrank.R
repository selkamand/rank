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
  expect_equal(smartrank(x, sort_by = "frequency", verbose = FALSE), c(1, 2, 2, 3, 3))
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
  expect_equal(smartrank(x, sort_by = "frequency", na.last = TRUE, verbose = FALSE), c(2, 4, 3, 5, 1, 2, 3))

  # Check the ranking when na.last is set to FALSE and sort_by is set to "frequency"
  expect_equal(smartrank(x, sort_by = "frequency", na.last = FALSE, verbose = FALSE), c(4, 1, 5, 2, 3, 4, 5))

  # NAs are dropped if na.last = NA
  expect_equal(smartrank(x, sort_by = "alphabetical", na.last = NA, verbose = FALSE), rank(x, na.last = NA))
  expect_equal(smartrank(x, sort_by = "frequency", na.last = NA, verbose = FALSE), c(2, 3, 1, 2, 3))


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

  # Test categorical input
  x <- c("a", "b", "c", "a", "b")
  expect_message(smartrank(x, sort_by = "frequency", verbose = TRUE), "smartrank: Sorting a categorical variable by frequency: ignoring ties.method\n")
  expect_message(smartrank(x, sort_by = "frequency", verbose = FALSE), NA)
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


