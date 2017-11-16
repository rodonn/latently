#' Chop off data.frame top and bottom ntile
#'
#' Takes a data.frame, splits it into ntiles and then returns only the top and bottom ntiles
#'
#' @param df a data.frame
#' @param variable unquoted name of the variable to divide into ntiles by
#' @param ntiles how many ntiles to split variable into
#'
#' @export
#'
chop_off_top_bottom_ntiles <- function(df, variable, ntiles) {
  variable <- enquo(variable)
  df %>%
    mutate(ntile = dplyr::ntile(!!variable, ntiles)) %>%
    filter(ntile %in% c(1, ntiles)) %>%
    mutate(top_ntile = ntile == max(ntile)) %>%
    mutate(bottom_ntile = ntile == 1)
}

#' Chop off top and bottom ntile factor loadings by factor
#'
#' @param df a data.frame in long format with at least three columns: "item_id", "factor" and "loading"
#' @param ntiles how many ntiles to split the factor loadings into
#'
chop_off_top_bottom_loading_ntiles_by_factor <- function(factor_df, ntiles) {
  factor_df %>%
    group_by(factor_id) %>%
    chop_off_top_bottom_ntiles(., loading, ntiles) %>%
    ungroup
}
