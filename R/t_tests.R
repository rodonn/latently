#' t-test heatmap
#'
#' Tests for differences in mean factor loadings between top and bottom ntile
#' within a set of covariates
#'
#' @param factor_df a data.frame in long format with at least three columns: "item_id", "factor" and "loading"
#' @param covariate_df a data.frame with covariates that will join against factor_df on item_id
#' @param covariates character vector of covariate names to perform the t-tests within
#' @param ntiles how many ntiles to split the factor loadings into
#' @export
t_test_heatmap <- function(factor_df, covariate_df, covariates, ntiles = 10) {
  # split the items into ntiles, then throw away everything but the top and bottom ntile
  factor_df %>%
    get_top_bottom_ntile_by_factor(ntiles) -> items_top_bottom

  # take any factor variables among the covariates and one-hot-encode them
  covariate_formula <- as.formula(paste0('~ ', paste(covariates, collapse = '+'), ' -1'))
  covariate_df %>%
    model.matrix(covariate_formula, .) %>%
    as.data.frame() %>%
    dplyr::bind_cols(covariate_df %>% select(item_id), .) -> covariate_mm

  # take the covariates from the covariate data.frame, then recast to long
  covariate_mm %>%
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
    ggplot(aes(factor_id, covariate, fill = abs(statistic))) +
    geom_tile() +
    xlab('latent factor #') + ylab('') +
    labs(title = paste0("difference in mean between top and bottom ",
                        ntile_name(ntiles),
                        ",\n shaded by abs(t-statistic)")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_viridis_c(name = 'abs(t-statistic)') +
    geom_text(aes(label = round(estimate, 2)), color = 'white', size = 2) +
    coord_cartesian(xlim = c(1, n_factors))
}
