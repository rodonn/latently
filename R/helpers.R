#' ntile name
#'
#' Returns the name of an ntile, e.g. "quartile" if passed 4
#'
#' @param ntile an integer
#'
ntile_name <- function(ntile) {
  ntile_name_out <- c(NA, NA, 'tercile', 'quartile', 'quintile', NA, NA, NA, NA, 'decile')[ntile]
  if(is.na(ntile_name_out)) {
    stop("Don't know what that ntile is called")
  } else {
    ntile_name_out
  }
}
