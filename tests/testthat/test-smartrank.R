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


test_that("desc flag works as expected", {
  x <- c(1, 3, 2, 5, 4)
  # Check the ranking when desc is set to TRUE
  expect_equal(smartrank(x, desc = TRUE, verbose = FALSE), c(5, 3, 4, 1, 2))
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
})


test_that("'na.last' works as expected for categorical variables", {
  x <- c("a", NA, "b", NA, "c", "a", "b")

  # Check the ranking when na.last is set to TRUE and sort_by is set to "alphabetical"
  expect_equal(smartrank(x, sort_by = "alphabetical", na.last = TRUE, verbose = FALSE), rank(x, na.last = TRUE))

  # Check the ranking when na.last is set to FALSE and sort_by is set to "alphabetical"
  expect_equal(smartrank(x, sort_by = "alphabetical", na.last = FALSE, verbose = FALSE), rank(x, na.last = FALSE))

  # Check the ranking when na.last is set to TRUE and sort_by is set to "frequency"
  expect_equal(smartrank(x, sort_by = "frequency", na.last = TRUE, verbose = FALSE), c(2, 4, 3, 5, 1, 2, 3))

  # Check the ranking when na.last is set to FALSE and sort_by is set to "frequency"
  expect_equal(smartrank(x, sort_by = "frequency", na.last = FALSE, verbose = FALSE), c(4, 1, 5, 2, 3, 4, 5))


  # What about when input vec is all NAs
  x <- c(NA, NA, NA)
  expect_equal(smartrank(x, sort_by = "frequency", na.last = TRUE, verbose = FALSE), c(1, 2, 3))
  expect_equal(smartrank(x, sort_by = "frequency", na.last = FALSE, verbose = FALSE), c(1, 2, 3))
  expect_equal(smartrank(x, sort_by = "alphabetical", na.last = TRUE, verbose = FALSE), c(1, 2, 3))
  expect_equal(smartrank(x, sort_by = "alphabetical", na.last = FALSE, verbose = FALSE), c(1, 2, 3))

})


