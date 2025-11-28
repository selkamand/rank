#' Rank a character vector based on supplied priority values
#'
#'
#' @param x A character vector.
#' @param priority_values A character vector descibing "priority" values.  Elements of `x` matching
#'               `priority_values` will be ranked based on their order of appearance in `priority_values`
#'
#' @return A vector of ranks describing `x` such that \code{x[order(ranks)]}
#' will move `priority_values` to the front of the vector
#'
#' @examples
#' x <- c("A", "B", "C", "D", "E")
#' rank_by_priority(x, c("C", "A"))
#' #> [1] "2" "4" "1" "4" "4"
#'
#' rank_by_priority(1:6, c(4, 2, 7))
#' #> [1] 4 2 1 3 5 6
#'
rank_by_priority <- function(x, priority_values, ties.method = "average") {

  # Assertions
  if(!is.character(x) & !is.numeric(x)) stop("rank_by_priority only works on character or numeric vectors")
  if(!is.character(priority_values) & !is.numeric(priority_values)) stop("rank_by_priority only works if `priority_values` is a character or numeric vector")
  if(is.numeric(x) != is.numeric(priority_values)) stop("rank_by_priority only works if `x` and 'priority_values' are of the same type")
  if(is.character(x) != is.character(priority_values)) stop("rank_by_priority only works if `x` and 'priority_values' are of the same type")

  # If no priorities requested just return indices so x[order(ranks)] keeps order identical
  if (length(priority_values) == 0) {
    return(seq_along(x))
  }

  # Get rid of values in priority_values that are not in X
  priority_values <- intersect(priority_values, x)

  # compute initial match-based ranks
  ranks <- match(x, priority_values)
  n_total   <- length(ranks)
  n_missing <- sum(is.na(ranks))

  # assign the same rank to all non-prioritised elements
  missing_idx <- which(is.na(ranks))
  ranks[missing_idx] <- n_total - n_missing + 1L

  # Use rank method - won't change anything just resolve ties (missing data) based on ties.method
  ranks <- rank(ranks, ties.method = ties.method)

  return(ranks)
}
