#' Top n by factor
#'
#' Returns the top n items/users by loading
#'
#' @param factor_df a data.frame in long format with at least three columns: "item_id", "factor" and "loading"
#' @param covariate_df a data.frame with covariates that will join against factor_df on item_id
#' @param include_columns character vector of variable names to include in the table
#' @param n how many items to return per factor

#' @export
#'
top_n_table <- function(factor_df, covariate_df, include_columns = NULL, n = 10) {
  factor_df %>%
    left_join(covariate_df, by='item_id') %>%
    arrange(factor_id, desc(loading)) %>%
    group_by(factor_id) %>%
    top_n(n, loading) %>%
    ungroup %>%
    select(one_of(c('factor_id', 'loading', include_columns)))
}
