#' Stratified hierarchical ranking across multiple variables
#'
#' `rank_stratified()` computes a single, combined rank for each row of a
#' data frame using **stratified hierarchical ranking**.
#' The first variable is ranked globally; each subsequent variable is then
#' ranked **within strata defined by all previous variables**.
#'
#' This is useful when you want a "truly hierarchical" ordering where,
#' for example, rows are first grouped and ordered by the frequency of
#' `gender`, and then within each `gender` group, ordered by the frequency
#' of `pet` **within that gender**, rather than globally.
#'
#' The result is a single rank vector that can be passed directly to
#' [base::order()] to obtain a stratified, multi-level
#' ordering.
#'
#' @param data A data frame. Each selected column
#'   represents one level of the stratified hierarchy, in the order given by
#'   `cols`.
#' @param cols Optional column specification indicating which variables in `data`
#'   to use for ranking, and in what order. Can be:
#'   \itemize{
#'     \item `NULL` (default): use all columns of `data` in their existing order.
#'     \item A character vector of column names.
#'     \item An integer vector of column positions.
#'   }
#' @param sort_by Character scalar or vector specifying how to rank each
#'   non-numeric column. Each element must be either `"alphabetical"` or
#'   `"frequency"`, matching the behaviour of [smartrank()]. If a single
#'   value is supplied it is recycled for all columns. For numeric columns,
#'   `sort_by` is ignored and ranking is always based on numeric order.
#' @param desc Logical scalar or vector indicating whether to rank each column
#'   in descending order. If a single value is supplied it is recycled for all
#'   columns.
#' @param ties.method Passed to [base::rank()] when resolving ties at each
#'   level; must be one of `"average"`, `"first"`, `"last"`, `"random"`,
#'   `"max"`, or `"min"`. See [base::rank()] for details.
#' @param na.last Logical, controlling the treatment of missing values,
#'   as in [base::rank()]. If `TRUE`, `NA`s are given the largest ranks; if
#'   `FALSE`, the smallest. Unlike [base::rank()] or [smartrank()], `na.last`
#'   cannot be set to `NA` in `rank_stratified()`, because dropping rows would
#'   change group membership and break stratified ranking.
#' @param verbose Logical; if `TRUE`, emit messages when `sort_by` is ignored
#'   (e.g. for numeric columns), mirroring the behaviour of [smartrank()].
#' @param freq_tiebreak Character scalar or vector controlling how
#'   alphabetical tie-breaking works when `sort_by = "frequency"` and the
#'   column is character/factor/logical. Each element must be one of:
#'   \itemize{
#'     \item `"match_desc"` (default): alphabetical tie-breaking follows
#'       `desc` for that column (ascending when `desc = FALSE`, descending
#'       when `desc = TRUE`).
#'     \item `"asc"`: ties are always broken by ascending alphabetical order.
#'     \item `"desc"`: ties are always broken by descending alphabetical order.
#'   }
#'   If a single value is supplied, it is recycled for all columns.
#'
#' @details
#' Stratified ranking proceeds level by level:
#'
#' \enumerate{
#'   \item The first selected column is ranked globally, using `sort_by[1]`
#'         (for non-numeric) and `desc[1]`.
#'   \item For the second column, ranks are computed **separately within each
#'         distinct combination of values of all previous columns**. Within each
#'         stratum, the second column is ranked using `sort_by[2]` / `desc[2]`.
#'   \item This process continues for each subsequent column: at level *k*,
#'         ranking is done within strata defined by columns 1, 2, ..., *k-1*.
#' }
#'
#' This yields a single composite rank per row that reflects a "true"
#' hierarchical (i.e. stratified) ordering: earlier variables define strata, and later variables
#' are only compared **within** those strata (for example, by within-stratum
#' frequency).
#'
#' @return
#' A numeric vector of length `nrow(data)`, containing stratified ranks.
#' Smaller values indicate "earlier" rows in the stratified hierarchy.
#'
#' @examples
#' library(rank)
#'
#' data <- data.frame(
#'   gender = c("male", "male", "male", "male", "female", "female", "male", "female"),
#'   pet    = c("cat",  "cat",  "magpie", "magpie", "giraffe", "cat", "giraffe", "cat")
#' )
#'
#' # Stratified ranking: first by gender frequency, then within each gender
#' # by pet frequency *within that gender*
#' r <- rank_stratified(
#'   data,
#'   cols = c("gender", "pet"),
#'   sort_by = c("frequency", "frequency"),
#'   desc = TRUE
#' )
#'
#' data[order(r), ]
#'
#' @export
rank_stratified <- function(data, cols = NULL, sort_by = "frequency", desc = FALSE, ties.method = "average", na.last = TRUE, freq_tiebreak = "match_desc", verbose = TRUE){

  # Early assertions
  if (!is.data.frame(data)) stop(sprintf("rank_stratified `data` argument must be a data.frame, not a [%s]", toString(class(data))))
  if (!is.logical(na.last)) stop("`na.last` must be logical (TRUE/FALSE), or a logical vector recycled per column.")
  if (anyNA(na.last)) stop("`na.last` must not contain NA; only TRUE or FALSE are supported in rank_stratified().")

  # Subset & reorder data based on cols
  if (!is.null(cols)) {

    # 1. Reject zero-length cols
    if (length(cols) == 0) {
      stop("`cols` must contain at least one column name or position; use cols = NULL to rank all columns.")
    }

    # 2. Convert to column positions
    if (is.character(cols)) {
      missing <- setdiff(cols, colnames(data))
      if (length(missing) > 0) {
        stop(sprintf(
          "All `cols` must be present in `data`. Missing: [%s]. Valid columns: [%s]",
          toString(missing),
          toString(colnames(data))
        ))
      }
      cols_idx <- match(cols, colnames(data))

    } else if (is.numeric(cols)) {

      if (!all(cols %in% seq_len(ncol(data)))) {
        stop("Numeric `cols` must be valid column positions in `data`.")
      }
      cols_idx <- cols

    } else {
      stop(sprintf(
        "`cols` must be NULL, a character vector, or an integer vector, not [%s].",
        toString(class(cols))
      ))
    }

    # 3. Do the subsetting once, using validated column positions
    data <- data[, cols_idx, drop = FALSE]
  }

  ncols <- ncol(data)

  desc <- if (length(desc) == 1) rep(desc, times = ncols) else desc
  ties.method <- if (length(ties.method) == 1) rep(ties.method, times = ncols) else ties.method
  na.last <- if (length(na.last) == 1) rep(na.last, times = ncols) else na.last
  sort_by <- if (length(sort_by) == 1) rep(sort_by, times = ncols) else sort_by
  freq_tiebreak <- if (length(freq_tiebreak) == 1) rep(freq_tiebreak, times = ncols) else freq_tiebreak

  # Validate freq_tiebreak values per column
  freq_tiebreak <- vapply(
    freq_tiebreak,
    function(z) match.arg(z, c("match_desc", "asc", "desc")),
    character(1)
  )

  if (length(desc) != ncols)
    stop(sprintf("`desc` argument must be a length 1 or contain one value per column [%d], not [%d]", ncols, length(desc)))
  if (length(ties.method) != ncols)
    stop(sprintf("`ties.method` argument must be a length 1 or contain one value per column [%d], not [%d]", ncols, length(ties.method)))
  if (length(na.last) != ncols)
    stop(sprintf("`na.last` argument must be a length 1 or contain one value per column [%d], not [%d]", ncols, length(na.last)))
  if (length(sort_by) != ncols)
    stop(sprintf("`sort_by` argument must be a length 1 or contain one value per column [%d], not [%d]", ncols, length(sort_by)))
  if (length(freq_tiebreak) != ncols)
    stop(sprintf("`freq_tiebreak` argument must be length 1 or contain one value per column [%d], not [%d]", ncols, length(freq_tiebreak)))



  prev_ranks <- NULL
  for (colindex in seq_len(ncols)) {
    # Grab current smartrank options
    curr_column <- data[[colindex]]
    curr_desc <- desc[colindex]
    curr_ties.method <- ties.method[colindex]
    curr_na.last <- na.last[colindex]
    curr_sort_by  <- sort_by[colindex]
    curr_freq_tiebreak <- freq_tiebreak[colindex]

    # Get ranks taking previous column ranks into account
    if (!is.null(prev_ranks)) {
      # 1. Compute within-group ranks using smartrank
      ls_values_by_prevrank <- split(curr_column, prev_ranks)
      ls_ranks <- lapply(
        ls_values_by_prevrank,
        smartrank,
        sort_by = curr_sort_by,
        desc = curr_desc,
        ties.method = curr_ties.method,
        na.last = curr_na.last,
        freq_tiebreak = curr_freq_tiebreak,
        verbose = verbose
      )

      # 2. Rebuild a "within_ranks" vector in original row order
      within_ranks <- numeric(length(prev_ranks))
      for (group_name in names(ls_ranks)) {
        # rows that belonged to this previous rank
        idx <- which(prev_ranks == as.numeric(group_name))
        within_ranks[idx] <- ls_ranks[[group_name]]
      }


      # 3. Combine prev_ranks and within_ranks into a new global rank
      # where within_ranks only
      combined_ranks <- combine_ranks(prev_ranks, within_ranks)
      ranks <- rank(combined_ranks, na.last = curr_na.last, ties.method = curr_ties.method)
    }
    else{
      ranks = smartrank(
        curr_column, sort_by = curr_sort_by, desc = curr_desc, ties.method = curr_ties.method,
        na.last = curr_na.last, freq_tiebreak = curr_freq_tiebreak, verbose = verbose
      )
    }

    # Early exit when there are no ties left to break
    if (!anyDuplicated(ranks)) break

    # Otherwise set prev_ranks to ranks and proceed to next column
    prev_ranks <- ranks
  }

  return(ranks)
}

# Combine ranks using a base-shifting approach. Not used because
# of possible errors when ranking large numbers of independent variables
# combine_ranks_baseshift <- function(prev_ranks, curr_ranks){
#   maximum <- max(curr_ranks, na.rm = TRUE)
#   minimum <- min(curr_ranks, na.rm = TRUE)
#   range = maximum-minimum + 1
#   return(prev_ranks * range + curr_ranks)
# }


combine_ranks <- function(prev_ranks, curr_ranks) {

  n <- length(prev_ranks)
  if (n == 0L) return(numeric(0))

  # 1. Order by prev, then curr (hierarchical comparison)
  ord <- order(prev_ranks, curr_ranks)
  prev_sorted <- prev_ranks[ord]
  curr_sorted <- curr_ranks[ord]

  # 2. Detect when the (prev, curr) pair changes (with tolerance)
  is_new_group <- logical(n)
  is_new_group[1L] <- TRUE

  for (i in 2L:n) {

    # new group if either prev or curr changed "meaningfully"

    is_new_group[i] <- (prev_sorted[i] != prev_sorted[i - 1L]) || (curr_sorted[i] != curr_sorted[i - 1L])
  }

  # 3. Cumulative groups -> ranks
  rank_sorted <- cumsum(is_new_group)

  # 4. Put back in original row order
  out <- numeric(n)
  out[ord] <- rank_sorted
  out
}

