test_that("reorder_by_priority brings specified character values to the front", {
  x  <- c("A", "B", "C", "D", "E")
  pv <- c("C", "A")

  res <- reorder_by_priority(x, pv)

  expect_equal(res, c("C", "A", "B", "D", "E"))
})

test_that("reorder_by_priority brings specified numeric values to the front", {
  x  <- 1:6
  pv <- c(4, 2, 7)  # 7 not in x

  res <- reorder_by_priority(x, pv)

  expect_equal(res, c(4, 2, 1, 3, 5, 6))
})

test_that("reorder_by_priority preserves order of non-priority elements", {
  x  <- letters[1:8]
  pv <- c("d", "b")

  res <- reorder_by_priority(x, pv)

  # After d and b, the rest should be in original order
  expect_equal(res, c("d", "b", "a", "c", "e", "f", "g", "h"))
})

test_that("reorder_by_priority ignores values not present in vec", {
  x  <- c("A", "B", "C", "D")
  pv <- c("X", "B", "Y")

  res <- reorder_by_priority(x, pv)

  expect_equal(res, c("B", "A", "C", "D"))
})

test_that("reorder_by_priority handles duplicate priority values", {
  x  <- c("A", "B", "C", "D")
  pv <- c("C", "A", "C")

  res <- reorder_by_priority(x, pv)

  # Behaviour should be as if pv == c("C", "A")
  expect_equal(res, c("C", "A", "B", "D"))
})

test_that("reorder_by_priority with empty priorities is a no-op", {
  x <- c("A", "B", "C")

  res <- reorder_by_priority(x, character())

  expect_identical(res, x)
})

test_that("reorder_by_priority is stable with NA values", {
  x  <- c("A", NA, "B", NA, "C")
  pv <- c("C", "A")

  res <- reorder_by_priority(x, pv)

  expect_equal(res, c("C", "A", NA, "B", NA))
})
