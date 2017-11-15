#' t-test heatmap
#'
#' Tests for differences in mean factor loadings between top and bottom ntile
#' within a set of covariates
#'
#' @param factor_df a data.frame in long format with at least three columns: "item_id", "factor" and "loading"
#' @param covariate_df a data.frame with covariates that will join against factor_df on item_id
#' @param include_covariates character vector of covariate names to perform the t-tests within
#' @param ntiles how many ntiles to split the factor loadings into
#' @export
t_test_heatmap <- function(factor_df, covariate_df, include_covariates, ntiles = 10) {
  # split the items into ntiles, then throw away everything but the top and bottom ntile
  factor_df %>%
    get_top_bottom_ntile_by_factor(ntiles) -> items_top_bottom

  # take the covariates from the covariate data.frame, then recast to long
  item_covariates %>%
    select(one_of(c('item_id', include_covariates))) %>%
    tidyr::gather(covariate, value, -item_id) -> item_covariates_long

  # join factor loadings and covariates, then perform t-tests for differences
  # in mean loading between top and bottom ntile within covariate
  items_top_bottom %>%
    left_join(item_covariates_long, by='item_id') %>%
    group_by(factor_id, covariate) %>%
    # estimates are top ntile - bottom ntile
    do(broom::tidy(t.test(value ~ bottom_ntile, data = .))) %>%
    ungroup -> t_tests

  n_factors <- length(unique(factor_df$factor_id))

  # plot the result of the above tests as a heatmap
  t_tests %>%
    ggplot(aes(factor_id, covariate, fill = statistic)) +
    geom_tile() +
    xlab('latent factor #') + ylab('') +
    labs(title = paste0("difference in mean between top and bottom ",
                        ntile_name(ntiles),
                        ",\n shaded by t-statistc")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_viridis_c(name = 't-statistic') +
    geom_text(aes(label = round(estimate, 2)), color = 'white', size = 2) +
    coord_cartesian(xlim = c(1, n_factors))
}
