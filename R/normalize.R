#' Normalize factor loadings
#'
#' Normalize factor loadings using different methods
#'
#' @param factor_df a data.frame in long format with at least three columns: "user_id" or "item_id", "factor" and "loading"
#' @param method the normalization method. Possible options:
#' - raw
#' - demean
#' - exp_normalized
#' - percent
#'
#' @export
#'
normalize_factor_loadings <- function(factor_df, method){
  valid_methods <- c('raw', 'demean','exp', 'percent')
  if(!(method %in% valid_methods)) {
    stop('normalization_method must be raw, demean, exp_normalized or percent')
  }

  if(!is.null(attr(factor_df, 'normalization')) && attr(factor_df, 'normalization') != 'raw') {
    stop('factor_df has already been normalized')
  }

  grouping_regex <- '(item|user)\\_id'

  if(method == 'raw')  {
    factor_df -> factor_df_out
  } else {
    factor_df %>%
      group_by_at(vars(matches(grouping_regex))) -> factor_grouped

    if(method == 'demean'){
      factor_grouped %>%
        mutate(loading = loading - mean(loading)) %>%
        ungroup -> factor_df_out
    } else if(method == 'exp'){
      factor_grouped %>%
        mutate(loading =  exp(loading - max(loading)) / sum(exp(loading - max(loading)))) %>%
        ungroup -> factor_df_out
    } else if(method == 'percent'){
      factor_grouped %>%
        mutate(loading =  loading / sum(loading)) %>%
        ungroup -> factor_df_out
    }
  }

  # attach the normalization that was applied to the data.frame, to be used by e.g. plotting methods
  attr(factor_df_out, 'normalization') <- method

  factor_df_out
}
