#' Normalize PFMUL factor loadings by total user loadings
#'
#' @param factor_df a data.frame in long format with at least three columns: "user_id" or "item_id", "factor_id" and "loading"
#' @param normalize_by_factor_df the data.frame of factor loadings to normalize by in long format. Must have columns "user_id" or "item_id", "factor_id" and "loading"
#'
#' @export
#'
user_normalize_pfmul_loadings <- function(factor_df, normalize_by_factor_df) {
  # calculate the total user loadings on each factor
  normalize_by_factor_df %>%
    group_by(factor_id) %>%
    summarize(total_loading = sum(loading)) -> total_factor_loadings

  # if factor_df and normalize_by_factor_df are identical (e.g. if item loadings
  # are to be normalized by total item loadings) normalize by _dividing_ by the
  # total factor loadings. If they differ, _multiply_
  operator <- ifelse(identical(factor_df, normalize_by_factor_df), '/', '*')

  # divide each user/item loading by the total user loading within factor
  factor_df %>%
    left_join(total_factor_loadings, by = 'factor_id') %>%
    mutate(loading = do.call(operator, list(loading, total_loading))) %>%
    select(-total_loading) -> factor_df_out

  attr(factor_df_out, 'user_normalized') <- TRUE

  factor_df_out
}

#' Normalize PFMUL factor loadings within user/item
#'
#' @param factor_df a data.frame in long format with at least three columns: "user_id" or "item_id", "factor_id" and "loading"
#'
#' @export
#'
frequency_normalize_pfmul_loadings <- function(factor_df) {
  factor_df %>%
    group_by_at(vars(matches('(user|item)_id'))) %>%
    mutate(loading = loading / sum(loading)) %>%
    ungroup  -> factor_df_out

  attr(factor_df_out, 'frequency_normalized') <- TRUE

  factor_df_out
}
