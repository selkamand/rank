test_that("rank_by_priority prioritises specified character values in order", {
  x  <- c("A", "B", "C", "D", "E")
  pv <- c("C", "A")

  r <- rank_by_priority(x, pv)

  # Using the ranks to reorder should bring priorities to the front
  expect_equal(x[order(r)], c("C", "A", "B", "D", "E"))
})

test_that("rank_by_priority prioritises specified numeric values in order", {
  x  <- 1:6
  pv <- c(4, 2, 7)  # 7 not in x

  r <- rank_by_priority(x, pv)

  # Reordering by rank should bring 4 then 2, then the rest in original order
  expect_equal(x[order(r)], c(4, 2, 1, 3, 5, 6))
})

test_that("non-priority elements are tied and remain in original order", {
  x  <- letters[1:8]
  pv <- c("d", "b")  # priorities: d then b

  r <- rank_by_priority(x, pv)

  # All non-priorities should have the same rank
  non_priority_idx <- which(!x %in% pv)
  non_priority_ranks <- unique(r[non_priority_idx])
  expect_equal(length(non_priority_ranks), 1L)

  # Reordering by rank should preserve relative order of non-priorities
  reordered <- x[order(r)]
  expect_equal(
    reordered,
    c("d", "b", "a", "c", "e", "f", "g", "h")  # a,c,e,f,g,h in original order
  )
})

test_that("priority_values entries not present in x are ignored", {
  x  <- c("A", "B", "C")
  pv <- c("X", "B", "Y", "A")

  r <- rank_by_priority(x, pv)

  # Only A and B matter; X, Y are ignored because they are not in x
  expect_equal(x[order(r)], c("B", "A", "C"))
})

test_that("duplicate entries in priority_values only use first occurrence", {
  x  <- c("A", "B", "C")
  pv <- c("C", "A", "C")  # duplicate "C"

  r <- rank_by_priority(x, pv)

  # Behaviour should be as if pv == c("C", "A")
  expect_equal(x[order(r)], c("C", "A", "B"))
})

test_that("empty priority_values returns identity ranking", {
  x <- c("A", "B", "C")

  r <- rank_by_priority(x, character())

  # Should be a "do nothing" rank vector
  expect_equal(r, seq_along(x))
  expect_equal(x[order(r)], x)
})

test_that("rank_by_priority handles all elements non-priority", {
  x  <- c("A", "B", "C")
  pv <- c("X", "Y")

  r <- rank_by_priority(x, pv)

  # If nothing matches, all ranks should be identical
  expect_equal(length(unique(r)), 1L)
  # And order should be unchanged
  expect_equal(x[order(r)], x)
})

test_that("rank_by_priority errors on unsupported types", {
  expect_error(
    rank_by_priority(list(1, 2, 3), c(1, 2)),
    regexp = "only works on character or numeric vectors"
  )

  expect_error(
    rank_by_priority(1:3, list(1, 2)),
    regexp = "only works if `priority_values` is a character or numeric vector"
  )
})

test_that("rank_by_priority errors when x and priority_values types differ", {
  expect_error(
    rank_by_priority(1:3, c("1", "2")),
    regexp = "the same type"
  )

  expect_error(
    rank_by_priority(c("a", "b"), c(1, 2)),
    regexp = "the same type"
  )
})

test_that("rank_by_priority is stable with respect to NA in x", {
  x  <- c("A", NA, "B", NA, "C")
  pv <- c("C", "A")

  r <- rank_by_priority(x, pv)
  reordered <- x[order(r)]

  # priorities first (C, A), then the rest (NA, B, NA) in original order
  expect_equal(reordered, c("C", "A", NA, "B", NA))
})
