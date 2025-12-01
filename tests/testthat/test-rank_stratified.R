test_that("rank_stratified with one column matches smartrank", {
  fruits <- c("Apple", "Orange", "Apple", "Pear", "Orange")
  df <- data.frame(fruits = fruits)

  r1 <- rank_stratified(df, sort_by = "frequency", desc = FALSE)
  r2 <- smartrank(df$fruits, sort_by = "frequency", desc = FALSE)

  expect_equal(r1, r2)
})

test_that("rank_stratified respects stratified frequency (gender / pet example)", {
  data <- data.frame(
    gender = c("male", "male", "male", "male", "female", "female", "male", "female"),
    pet    = c("cat",  "cat",  "magpie", "magpie", "giraffe", "cat", "giraffe", "cat")
  )

  r <- rank_stratified(
    data,
    sort_by = c("frequency", "frequency"),
    desc    = c(TRUE, TRUE),
  )

  ord <- order(r)

  expect_equal(
    data$gender[ord],
    c("male", "male", "male", "male", "male", "female", "female", "female")
  )

  expect_equal(
    data$pet[ord],
    c("magpie", "magpie", "cat", "cat", "giraffe", "cat", "cat", "giraffe")
  )
})

test_that("rank_stratified differs from global lexicographic ranking when frequencies differ by group", {
  data <- data.frame(
    gender = c("male", "male", "male", "male", "female", "female", "male", "female"),
    pet    = c("cat",  "cat",  "magpie", "magpie", "giraffe", "cat", "giraffe", "cat")
  )

  # Lexicographic: smartrank applied independently to each column
  r_gender <- smartrank(data$gender, sort_by = "frequency", desc = TRUE)
  r_pet    <- smartrank(data$pet,    sort_by = "frequency", desc = TRUE)
  ord_lex  <- order(r_gender, r_pet)

  # Stratified: pet frequencies computed within gender
  r_strat  <- rank_stratified(
    data,
    sort_by = c("frequency", "frequency"),
    desc    = c(TRUE, TRUE)
  )
  ord_strat <- order(r_strat)

  # They shouldn't give exactly the same ordering
  expect_false(identical(ord_lex, ord_strat))
})

test_that("later columns only break ties from previous ranks", {
  # First column has two groups "A" < "B"
  # Second column orders within A and within B
  data <- data.frame(
    group = c("A", "A", "B", "B"),
    value = c("z", "a", "a", "z")
  )

  # stratified alphabetical ranking on both columns
  r <- rank_stratified(
    data,
    sort_by = c("alphabetical", "alphabetical"),
    desc    = c(FALSE, FALSE)
  )

  ord <- order(r)

  # All "A" rows must appear before all "B" rows
  expect_equal(
    data$group[ord],
    c("A", "A", "B", "B")
  )

  # Within A, "a" should come before "z"
  expect_equal(
    data$value[ord][data$group[ord] == "A"],
    c("a", "z")
  )

  # Within B, "a" should come before "z"
  expect_equal(
    data$value[ord][data$group[ord] == "B"],
    c("a", "z")
  )
})

test_that("rank_stratified stops effectively when first column has no ties", {
  data <- data.frame(
    x = c(1, 3, 2),
    y = c("a", "b", "c")  # irrelevant if x has no ties
  )

  r_strat <- rank_stratified(
    data,
    sort_by = c("alphabetical", "alphabetical"),
    desc    = c(FALSE, FALSE)
  )

  r_x <- smartrank(data$x, sort_by = "alphabetical", desc = FALSE)

  # If first column has all unique values, later columns should not change the order
  expect_equal(r_strat, r_x)
})

test_that("rank_stratified recycles single sort_by/desc across columns", {
  data <- data.frame(
    x = c("b", "a", "b"),
    y = c("c", "c", "a")
  )

  # Just check it runs and returns a rank of correct length and type
  r <- rank_stratified(
    data,
    sort_by = "alphabetical",
    desc    = FALSE
  )

  expect_true(is.numeric(r))
  expect_length(r, nrow(data))
})

test_that("rank_stratified keeps ties when rows are identical across all columns", {
  df <- data.frame(
    g = c("A", "A", "A"),
    x = c("x", "x", "x")
  )

  r <- rank_stratified(
    df,
    cols    = c("g", "x"),
    sort_by = c("alphabetical", "alphabetical"),
    desc    = c(FALSE, FALSE)
  )

  # All rows identical -> all ranks should be identical
  expect_equal(length(unique(r)), 1L)
})

test_that("rank_stratified keeps ties within strata when later columns add no information", {
  df <- data.frame(
    group = c("A", "A", "B", "B"),
    value = c("x", "x", "y", "y")
  )

  r <- rank_stratified(
    df,
    cols    = c("group", "value"),
    sort_by = c("alphabetical", "alphabetical"),
    desc    = c(FALSE, FALSE)
  )

  ord <- order(r)

  # Rows 1 and 2 (group A, value x) should have the same rank
  expect_equal(
    unique(r[ord][df$group[ord] == "A"]),
    r[ord][df$group[ord] == "A"][1]
  )

  # Rows 3 and 4 (group B, value y) should have the same rank
  expect_equal(
    unique(r[ord][df$group[ord] == "B"]),
    r[ord][df$group[ord] == "B"][1]
  )
})

test_that("rank_stratified with multiple columns can still produce ties overall", {
  df <- data.frame(
    g  = c("A", "A", "B", "B"),
    x1 = c("x", "x", "y", "y"),
    x2 = c("z", "z", "w", "w")
  )

  r <- rank_stratified(
    df,
    cols    = c("g", "x1", "x2"),
    sort_by = c("alphabetical", "alphabetical", "alphabetical"),
    desc    = c(FALSE, FALSE, FALSE)
  )

  # There should be at least some duplicated ranks
  expect_true(anyDuplicated(r) > 0)
})
