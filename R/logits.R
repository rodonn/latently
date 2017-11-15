#' logit heatmap
#'
#' Tests for differences in mean factor loadings between top and bottom ntile by running logit regressions on a set of covariates
#'
#' @param factor_df a data.frame in long format with at least three columns: "item_id", "factor" and "loading"
#' @param covariate_df a data.frame with covariates that will join against factor_df on item_id
#' @param covariates character vector of covariate names to be used as independent variables in the logit
#' @param ntiles how many ntiles to split the factor loadings into
#' @export
logit_heatmap <- function(factor_df, covariate_df, covariates, ntiles = 10) {
  # split the items into ntiles, then throw away everything but the top and bottom ntile
  factor_df %>%
    get_top_bottom_ntile_by_factor(ntiles) -> items_top_bottom

  # join factor loadings and covariates, then perform logits for membership in the top ntile
  logit_formula <- as.formula(paste0('top_ntile ~ ', paste(covariates, collapse = ' + ')))

  items_top_bottom %>%
    left_join(covariate_df, by='item_id') %>%
    group_by(factor_id) %>%
    # estimates are top ntile - bottom ntile
    do(broom::tidy(glm(logit_formula,
                       data = .,
                       family = binomial(link = logit)))) %>%
    ungroup -> logit_results

  n_factors <- length(unique(factor_df$factor_id))

  # plot the result of the above logits as a heatmap
  logit_results %>%
    filter(term != '(Intercept)') %>%
    ggplot(aes(factor_id, term, fill = statistic)) +
    geom_tile() +
    xlab('latent factor #') + ylab('') +
    labs(title = paste0("difference in mean between top and bottom ",
                        ntile_name(ntiles),
                        ",\n shaded by z-statistic")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_viridis_c(name = 'z-statistic') +
    geom_text(aes(label = round(estimate, 2)), color = 'white', size = 2) +
    coord_cartesian(xlim = c(1, n_factors))
}
