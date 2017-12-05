#' logit heatmap
#'
#' Tests for differences in mean factor loadings between top and bottom ntile by running logit regressions on a set of covariates
#'
#' @param factor_df a data.frame in long format with at least three columns: "item_id" or "user_id", "factor" and "loading"
#' @param covariate_df a data.frame with covariates that will join against factor_df on the ID column
#' @param covariates character vector of covariate names to be used as independent variables in the logit
#' @param ntiles how many ntiles to split the factor loadings into
#' @export
logit_heatmap <- function(factor_df, covariate_df, covariates, ntiles = 10) {
  # call run_logits() with the same arguments the current function was called with
  Call <- sys.call()
  Call[[1]] <- run_logits
  logit_results <- eval(Call, parent.frame())

  n_factors <- length(unique(logit_results$factor_id))

  # plot the result of the above logits as a heatmap
  logit_results %>%
    filter(term != '(Intercept)') %>%
    ggplot2::ggplot(aes(factor_id, term, fill = statistic)) +
    ggplot2::geom_tile() +
    ggplot2::xlab('latent factor #') + ylab('') +
    ggplot2::labs(caption = paste0("difference in mean between top and bottom ",
                                   ntile_name(ntiles),
                                   ",\n test statistic as text, shaded by z-statistic")) +
    ggplot2::scale_fill_distiller(palette = "RdBu",
                                  limits = c(-1,1) * max(abs(logit_results$statistic)),
                                  name = 'z-statistic',
                                  direction = -1) +
    ggplot2::geom_text(aes(label = round(estimate, 2)), color = 'white', size = 2) +
    ggplot2::coord_cartesian(xlim = c(1, n_factors))
}

#' Logits for differences in covariates
#'
#' Performs the logit estimations for logit_heatmap
#'
#' @param factor_df a data.frame in long format with at least three columns: "item_id" or "user_id", "factor" and "loading"
#' @param covariate_df a data.frame with covariates that will join against factor_df on the ID column
#' @param covariates character vector of covariate names to perform the t-tests within
#' @param ntiles how many ntiles to split the factor loadings into
#'
run_logits <- function(factor_df, covariate_df, covariates, ntiles) {
  id_col <- get_id_col(factor_df)

  # split the items into ntiles, then throw away everything but the top and bottom ntile
  factor_df %>%
    chop_off_top_bottom_loading_ntiles_by_factor(ntiles) -> items_top_bottom

  # join factor loadings and covariates, then perform logits for membership in the top ntile
  items_top_bottom %>%
    left_join(covariate_df, by = id_col) %>%
    group_by(factor_id) %>%
    # estimates are top ntile - bottom ntile
    do(logit_predict_top_ntile(.data, covariates)) %>%
    ungroup -> logit_results

  logit_results
}

#' Predict membership in the top ntile
#'
#' Predict membership in the top ntile given a set of covariates
#'
#' @param df a data.frame that must contain both a "top_ntile" variable and all covariates
#' @param covariates character vector of variable names
#'
#' @export
#'
logit_predict_top_ntile <- function(df, covariates) {
  logit_formula <- as.formula(paste0('top_ntile ~ ', paste(covariates, collapse = ' + ')))

  logit_estimates <- glm(logit_formula,
                         data = df,
                         family = binomial(link = logit))

  broom::tidy(logit_estimates)
}

