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
  # call perform_t_tests() with the same arguments the current function was called with
  Call <- sys.call()
  Call[[1]] <- perform_t_tests
  t_test_results <- eval(Call, parent.frame())

  n_factors <- length(unique(t_test_results$factor_id))

  # plot the result of the above tests as a heatmap
  t_test_results %>%
    ggplot2::ggplot(aes(factor_id, covariate, fill = statistic)) +
    ggplot2::geom_tile() +
    ggplot2::xlab('latent factor #') + ylab('') +
    ggplot2::labs(caption = paste0("difference in mean between top and bottom ",
                                    ntile_name(ntiles),
                                    ",\n test statistic as text, shaded by t-statistic")) +
    ggplot2::scale_fill_distiller(palette = "RdBu",
                         limits = c(-1,1) * max(abs(t_test_results$statistic)),
                         name = 't-statistic',
                         direction = -1) +
    ggplot2::geom_text(aes(label = round(estimate, 2)), color = 'white', size = 2) +
    ggplot2::coord_cartesian(xlim = c(1, n_factors))
}

#' t-test for differences in covariates
#'
#' Performs the t-tests for t_test_heatmap
#'
#' @param factor_df a data.frame in long format with at least three columns: "item_id", "factor" and "loading"
#' @param covariate_df a data.frame with covariates that will join against factor_df on item_id
#' @param covariates character vector of covariate names to perform the t-tests within
#' @param ntiles how many ntiles to split the factor loadings into
#'
perform_t_tests <- function(factor_df, covariate_df, covariates, ntiles) {
  # split the items into ntiles, then throw away everything but the top and bottom ntile
  factor_df %>%
    chop_off_top_bottom_loading_ntiles_by_factor(ntiles) -> items_top_bottom

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
    # kick out cells where there's no variation in the covariates
    filter(length(unique(value)) > 1) %>%
    # estimates are top ntile - bottom ntile
    do(broom::tidy(t.test(value ~ bottom_ntile, data = .))) %>%
    ungroup -> t_test_results

  t_test_results
}
