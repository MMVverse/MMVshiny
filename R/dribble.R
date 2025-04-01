#' Create data.frames rowwize.
#' @param listEntries a list. The first ncol elements of it will be converted to characters and 
#' used as column names. The remaining entries fill up each row from left to right.
#' @param ncol an integer indicating the number of columns. 
#' @param FLAG_data.table a logical indicating whether to use data.table::data.table instead of data.frame.
#' Default is FALSE. 
#' @details This is a replacer for `dplyr::tribble` which tolerates
#' characters and numerics in the same column. 
#' @examples
#' # The following call to dplyr::tribble would cause an error with dplyr v1.0.7
#' # dplyr::tribble(
#' #   ~Parameter,                                 ~Value,        ~Unit,
#' #   "Selected PK method",                    "Allometry",   "PK method",
#' #   "Species Available for Allometry",                 2,     "Species",
#' #   "Predicted plasma CL in man",                    3.2,   "mL/min/kg")
#' # # Error: Can't create column `Value`: Can't combine `..1` <character> 
#' # and `..2` <double>.
#'
#' # dribble runs without error and creates a list column Value. 
#' dribble(list(
#'    "Parameter",                                "Value",          "Unit",
#'    "Selected PK method",                   "Allometry",     "PK method",
#'    "Species Available for Allometry",                2,       "Species",
#'    "Predicted plasma CL in man",                   3.2,     "mL/min/kg"),
#'    ncol = 3)
#'
#' @return a `(length(listEntries)-ncol)/ncol x ncol` data.frame with column names 
#' `as.character(listEntries[1:ncol])` or a data.table of the same dimension if 
#' FLAG_data.table is set to TRUE. An error is raised if 
#' \code{length(listEntries) %% ncol != 0}.
#' @author Venelin Mitov (IntiQuan)
#' @export
dribble <- function(listEntries, ncol, FLAG_data.table = FALSE) {
  if(length(listEntries) %% ncol != 0) {
    stop("dribble: number of entries does not divide on ncol.")
  }
  colNames <- as.character(listEntries[1:ncol])
  listColumns <- lapply(seq_along(colNames), function(i) {
    column <- do.call(c, listEntries[seq(ncol + i, length(listEntries), by = ncol)])
    column
  })
  names(listColumns) <- colNames
  if(FLAG_data.table) {
    do.call(data.table, listColumns)
  } else {
    do.call(data.frame, c(listColumns, list(stringsAsFactors = FALSE)))
  }
}

