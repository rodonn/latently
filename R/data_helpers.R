#' Get top and bottom ntile items
#'
#' @param factor_df a data.frame in long format with at least three columns: "item_id", "factor" and "loading"
#' @param ntiles how many ntiles to split the factor loadings into
#'
get_top_bottom_ntile_by_factor <- function(factor_df, ntiles) {
  factor_df %>%
    group_by(factor_id) %>%
    mutate(loading_ntile = dplyr::ntile(loading, ntiles)) %>%
    ungroup %>%
    filter(loading_ntile %in% c(1, ntiles)) %>%
    mutate(top_ntile = loading_ntile == max(loadings_ntile)) %>%
    mutate(bottom_ntile = loading_ntile == 1)
}
