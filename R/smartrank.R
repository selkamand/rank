#' Rank a vector based on either alphabetical or frequency order
#'
#' This function acts as a drop-in replacement for the base `rank()` function with the added option to:
#' 1. Rank categorical factors based on frequency instead of alphabetically
#' 2. Rank in descending or ascending order
#'
#' @param x A numeric, character, or factor vector
#' @param sort_by Sort ranking either by "alphabetical" or "frequency" . Default is "alphabetical"
#' @param desc A logical indicating whether the ranking should be in descending ( TRUE ) or ascending ( FALSE ) order.
#'  When input is numeric, ranking is always based on numeric order.
#' @param verbose verbose (flag)
#' @inheritParams base::rank
#' @note When `sort_by = "frequency"`, ties based on frequency are broken by alphabetical order of the terms
#' @note When `sort_by = "frequency"` and input is character, ties.method is ignored. Each distinct element level gets its own rank, and each rank is 1 unit away from the next element, irrespective of how many duplicates
#'
#' @return The ranked vector
#' @details
#' If \code{x} includes ‘ties’ (equal values), the \code{ties.method} argument determines how the rank value is decided. Must be one of:
#' \itemize{
#'     \item \strong{average}: replaces integer ranks of tied values with their average  (default)
#'     \item \strong{first}: first-occurring value is assumed to be the lower rank (closer to one)
#'     \item \strong{last}: last-occurring value is assumed to be the lower rank (closer to one)
#'     \item \strong{max} or \strong{min}: integer ranks of tied values are replaced with their maximum and minimum respectively (latter is typical in sports-ranking)
#'     \item \strong{random} which of the tied values are higher / lower rank is randomly decided.
#'}
#'
#' NA values are never considered to be equal:
#' for na.last = TRUE and na.last = FALSE
#' they are given distinct ranks in the order in which they occur in x.
#'
#' @examples
#'
#' # ------------------
#' ## CATEGORICAL INPUT
#' # ------------------
#'
#' fruits <- c("Apple", "Orange", "Apple", "Pear", "Orange")
#'
#' # rank alphabetically
#' smartrank(fruits)
#' #> [1] 1.5 3.5 1.5 5.0 3.5
#'
#' # rank based on frequency
#' smartrank(fruits, sort_by = "frequency")
#' #> [1] 2.5 4.5 2.5 1.0 4.5
#'
#' # rank based on descending order of frequency
#' smartrank(fruits, sort_by = "frequency", desc = TRUE)
#' #> [1] 1.5 3.5 1.5 5.0 3.5
#'
#' # sort fruits vector based on rank
#' ranks <- smartrank(fruits,sort_by = "frequency", desc = TRUE)
#' fruits[order(ranks)]
#' #> [1] "Apple"  "Apple"  "Orange" "Orange" "Pear"
#'
#'
#' # ------------------
#' ## NUMERICAL INPUT
#' # ------------------
#'
#' # rank numerically
#' smartrank(c(1, 3, 2))
#' #> [1] 1 3 2
#'
#' # rank numerically based on descending order
#' smartrank(c(1, 3, 2), desc = TRUE)
#' #> [1] 3 1 2
#'
#' # always rank numeric vectors based on values, irrespective of sort_by
#' smartrank(c(1, 3, 2), sort_by = "frequency")
#' #> smartrank: Sorting a non-categorical variable. Ignoring `sort_by` and sorting numerically
#' #> [1] 1 3 2
#' @export
smartrank <- function(x, sort_by = c("alphabetical", "frequency"), desc = FALSE, ties.method = "average",  na.last = TRUE, verbose = TRUE) {

  # Assertions --------------------------------------------------------------
  if(identical(sort_by, c("alphabetical", "frequency"))) sort_by <- "alphabetical"
  if(!(is.character(sort_by) && length(sort_by) == 1)) stop("sort_by must be one of 'alphabetical' or 'frequency'")
  if(!sort_by %in% c('alphabetical', 'frequency')) stop("sort_by must be one of 'alphabetical' or 'frequency'")
  if(!(!is.null(na.last) & is.logical(na.last))) stop("na.last must be TRUE/FALSE")
  if(!(!is.null(verbose) && !is.na(verbose) && is.logical(verbose))) stop("verbose must be TRUE/FALSE")
  if(length(ties.method) > 1 || ! ties.method %in% c( "average", "first", "last", "random", "max", "min")) stop('ties.method should be one of: "average", "first", "last", "random", "max", or "min"')



  # Categorical Input -------------------------------------------------------
  if (is.character(x) || is.factor(x) || is.logical(x)) {
    if (sort_by == "alphabetical") {
      alphabetical_rank <- rank(x, na.last=na.last, ties.method = ties.method)

      # Sort alphabetically in descending order
      if(desc){
        max_rank <- ceiling(max(alphabetical_rank[!is.na(x)]))
        alphabetical_rank <- ifelse(
          test = !is.na(x),
          yes = max_rank - alphabetical_rank + 1,
          no = alphabetical_rank
        )
      }
      return(alphabetical_rank)
    } else if (sort_by == "frequency") {

      # Replicate behavour of base rank() where if na.last = NA, NA elements are dropped from vector
      if(is.na(na.last)) x <- x[!is.na(x)]

      # If vector is all NAs return 1:n
      if(all(is.na(x))) return(seq_along(x))

      x = as.character(x)

      # Create a table of the frequencies of each element
      # (by default this table is always sorted in ascending alphabetical order)
      freq_table <- as.data.frame(table(x))

      # Sort the table by the frequencies in descending order, then by alphabetic order.
      # The secondary sort by alphabetical order guarantees no ties)
      if(desc){
        freq  <-  -freq_table$Freq
        level_names <- rev(freq_table$x) # Reverse the alphabetical order
      }
      else{
        freq <- freq_table$Freq
        level_names <- freq_table$x
      }

      freq_table <- freq_table[order(freq, level_names),]

      # Get the numerical order of elements by frequency
      ranking <- match(x, freq_table$x)

      # Rank results (to apply ties.method and na.last options)
      ranking <- rank(ranking, na.last = na.last, ties.method = ties.method)

      return(ranking)
    }
    else
      stop("Author has forgotten to code a response when sort_by == {sort_by}")
  }
  # Non-categorical data
  else {
    if(verbose & sort_by == "frequency") { message("smartrank: Sorting a non-categorical variable. Ignoring `sort_by` and sorting numerically") }
    if(desc)
      ranking <- rank(-x, na.last = na.last, ties.method = ties.method)
    else
      ranking <- rank(x, na.last = na.last, ties.method = ties.method)
    return(ranking)
  }
}

