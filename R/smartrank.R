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
#' @return The ranked vector
#' @examples
#' smartrank(c("apple","banana","banana","orange","apple"))
#' smartrank(c("apple","banana","banana","orange","apple"),sort_by = "frequency")
#' @export
smartrank <- function(x, sort_by = c("alphabetical", "frequency"), desc = FALSE, ties.method = "average") {

  # Assertions --------------------------------------------------------------
  if(all(sort_by == c("alphabetical", "frequency"))) sort_by <- "alphabetical"
  if(!(is.character(sort_by) && length(sort_by) == 1)) stop("sort_by must be one of 'alphabetical' or 'frequency'")
  if(!sort_by %in% c('alphabetical', 'frequency')) stop("sort_by must be one of 'alphabetical' or 'frequency'")


  if (is.numeric(x)) {
    if(desc)
      ranking <- rank(-x)
    else
      ranking <- rank(x)
    return(ranking)
  }
  else if (is.character(x) || is.factor(x) || is.logical(x)) {
    if (sort_by == "alphabetical") {
      return(rank(x, ties.method = "min"))
    } else if (sort_by == "frequency") {

      x = as.character(x)

      # Create a table of the frequencies of each element
      freq_table <- as.data.frame(table(x))

      # Add an index column to the table
      freq_table$idx <- 1:nrow(freq_table)

      # Sort the table by the frequencies in descending order, and by alphabetic order then by original index in case of ties
      if(desc)
        freq  <-  -freq_table$Freq
      else
        freq <- freq_table$Freq

      freq_table <- freq_table[order(freq, freq_table$x, freq_table$idx),]

      # Get the original vector elements in the sorted order
      return(match(x, freq_table$x))
    }
    else
      stop("Author has forgotten to code a response when sort_by == {sort_by}")
  }
  else {
    stop("Input must be a numeric, character, or factor vector.")
  }
}
#
# arg_match <- function(x, choices) {
#   match_res <- match(x, choices)
#   if (is.na(match_res)) {
#     stop(paste0("'", x, "' is not an element of '", paste(choices, collapse = "', '"), "'"))
#   }
#   return(match_res)
# }

