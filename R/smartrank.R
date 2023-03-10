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
#' @note When `sort_by = "frequency"` and input is character, ties.method is ignored. each distinct element level gets its own rank, and each rank is 1 unit away from the next element, irrespective of how many duplicates
#'
#' @return The ranked vector
#' @examples
#'
#' ## CATEGORICAL INPUT -----------------------
#' fruits <- c("Apple", "Orange", "Apple", "Pear", "Orange")
#'
#' # rank alphabetically
#' smartrank(fruits)
#' #> [1] 1.5 3.5 1.5 5.0 3.5
#'
#' # rank based on frequency
#' smartrank(fruits, sort_by = "frequency")
#' #> smartrank: Sorting a categorical variable by frequency: ignoring ties.method
#' #> [1] 2 3 2 1 3
#'
#' # rank based on descending order of frequency
#' smartrank(fruits,sort_by = "frequency", desc = TRUE)
#' #> smartrank: Sorting a categorical variable by frequency: ignoring ties.method
#' #> [1] 1 2 1 3 2
#'
#'
#' ## NUMERICAL INPUT -----------------------
#'
#' # rank numerically
#' smartrank(c(1, 3, 2))
#' #> [1] 1 3 2
#'
#' # rank numerically based on descending order
#' smartrank(c(1, 3, 2), desc = TRUE)
#' #> [1] 3 1 2
#'
#' # always rank numerically, irrespective of sort_by
#' smartrank(c(1, 3, 2), sort_by = "frequency")
#' #> smartrank: Sorting a numeric variable. Ignoring `sort_by` and sorting numerically
#' #> [1] 1 3 2
#' @export
smartrank <- function(x, sort_by = c("alphabetical", "frequency"), desc = FALSE, ties.method = "average",  na.last = TRUE, verbose = TRUE) {

  # Assertions --------------------------------------------------------------
  if(all(sort_by == c("alphabetical", "frequency"))) sort_by <- "alphabetical"
  if(!(is.character(sort_by) && length(sort_by) == 1)) stop("sort_by must be one of 'alphabetical' or 'frequency'")
  if(!sort_by %in% c('alphabetical', 'frequency')) stop("sort_by must be one of 'alphabetical' or 'frequency'")
  if(!(!is.null(na.last) & is.logical(na.last))) stop("na.last must be TRUE/FALSE")
  if(!(!is.null(verbose) & !is.na(verbose) & is.logical(verbose))) stop("verbose must be TRUE/FALSE")
  if(! ties.method %in% c( "average", "first", "last", "random", "max", "min")) stop('ties.method should be one of: "average", "first", "last", "random", "max", or "min"')



  # Categorical Input -------------------------------------------------------
  if (is.character(x) || is.factor(x) || is.logical(x)) {
    if (sort_by == "alphabetical") {
      return(rank(x, na.last=na.last, ties.method = ties.method))
    } else if (sort_by == "frequency") {

      # TODO ensure it works well with na.last and treats NAs as expected
      if(verbose) message("smartrank: Sorting a categorical variable by frequency: ignoring ties.method")

      # Replicate behavour of base rank() where if na.last = NA, NA elements are dropped from vector
      if(is.na(na.last)) x <- x[!is.na(x)]

      # If vector is all NAs return 1:n
      if(all(is.na(x))) return(seq_along(x))

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

      # Get the numerical order of elements by frequency
      ranking <- match(x, freq_table$x)

      # Deal with NAs
      number_nas <- sum(is.na(ranking))

      if(number_nas > 0){
        if(na.last){
          max_rank <- max(ranking, na.rm = TRUE)

          ranking[is.na(ranking)] <- (max_rank+1):(max_rank + number_nas)
          #ranking <- ifelse(is.na(ranking), (max_rank+1):(max_rank + number_nas+1), no = ranking)
        }
        else{
          ranking <- ranking + number_nas
          ranking[is.na(ranking)] <- c(1:number_nas)
        }
      }
      return(ranking)
    }
    else
      stop("Author has forgotten to code a response when sort_by == {sort_by}")
  }
  else {
    if(verbose & sort_by == "frequency") { message("smartrank: Sorting a non-categorical variable. Ignoring `sort_by` and sorting numerically") }
    if(desc)
      ranking <- rank(-x, na.last = na.last, ties.method = ties.method)
    else
      ranking <- rank(x, na.last = na.last, ties.method = ties.method)
    return(ranking)
  }
}

