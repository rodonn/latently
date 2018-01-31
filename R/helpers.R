
ntile_names <- data.table::fread('
ntile_value, ntile_name
3, tercile
4, quartile
5, quintile
6, sextile
7, septile
8, octile
10, decile
16, hexadecile
20, viginitile
100, percentile
')

#' ntile name
#'
#' Returns the name of an ntile, e.g. "quartile" if passed 4
#'
#' @param ntile an integer
#'
ntile_name <- function(ntile) {
  ntile_name_out <- ntile_names[ntile == ntile_value, ntile_name][1]
  if(is.na(ntile_name_out)) {
    stop("Don't know what that ntile is called")
  } else {
    return( ntile_name_out)
  }
}



##' If dt has column old_name, then rename that column new_name by reference
##'
##' @param dt a data.table
##' @param old_name if dt has a column with this name, rename it new_name, otherwise do nothing
##' @param new_name the new name to replace old_name
replace_name <- function(dt, old_name, new_name) {
  if (old_name %in% names(dt)) {
    setnames(dt, old_name, new_name)
  }
}
