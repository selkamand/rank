#' Bring specified values in a vector to the front
#'
#' Reorders a vector so that any elements matching the values in `values`
#' appear first, in the order they appear in `values`. All remaining elements
#' are returned afterward, preserving their original order.
#'
#' @param x A character or numeric vector to reorder.
#' @param priority_values A vector of “priority” values. Elements of `x` that match
#'   entries in `priority_values` are moved to the front in the order they appear in
#'   `priority_values`. Values not found in `x` are ignored.
#'
#' @return A reordered vector with priority values first, followed by all
#'   remaining elements in their original order.
#'
#' @examples
#' reorder_by_priority(c("A", "B", "C", "D", "E"), c("C", "A"))
#' reorder_by_priority(1:6, c(4, 2, 7))
#'
#' @export
reorder_by_priority <- function(x, priority_values) {
  if (length(priority_values) == 0) return(x)

  ranks <- rank_by_priority(x, priority_values)
  x[order(ranks)]
}
