% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/t_tests.R
\name{t_test_heatmap}
\alias{t_test_heatmap}
\title{t-test heatmap}
\usage{
t_test_heatmap(factor_df, covariate_df, covariates, ntiles = 10)
}
\arguments{
\item{factor_df}{a data.frame in long format with at least three columns: "item_id" or "user_id", "factor" and "loading"}

\item{covariate_df}{a data.frame with covariates that will join against factor_df on the ID column}

\item{covariates}{character vector of covariate names to perform the t-tests within}

\item{ntiles}{how many ntiles to split the factor loadings into}
}
\description{
Tests for differences in mean factor loadings between top and bottom ntile
within a set of covariates
}
