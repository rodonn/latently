#' Read in PFMUL parameter file
#'
#' @param parameter_name the (character) name of the parameter to be read in, e.g. "alpha" for item factor loadings
#' @param moment the moment of the parameter to be read in, either "mean" or "std"
#' @param data_dir the directory in which the parameter tsv files reside
#' @param shape whether to return the parameters in long or wide format
#'
#' @export
read_pfmul_parameter_file <- function(parameter_name, moment = 'mean', data_dir, shape = 'long') {
  # read in the tsv file
  file_name <- paste0('param_', parameter_name, '_', moment, '.tsv')
  parameter_wide <- data.table::fread(file.path(data_dir, file_name))

  # set column names
  unit_id_name <- ifelse(parameter_name %in% c('alpha', 'beta'), 'item_id', 'user_id')
  factor_names <- factor_sequence(ncol(parameter_wide) - 2)
  colnames(parameter_wide) <- c('row', unit_id_name, factor_names)
  parameter_wide$row <- NULL

  if(shape == 'wide') {
    parameter_wide
  } else {
    parameter_wide %>%
      tidyr::gather(factor_name, loading, starts_with('factor')) %>%
      mutate(factor_id = factor_label_to_id(factor_name))
  }
}


#' strip a robust factor label to its id
#'
#' @param factor_label a character vector of robust factor labels, e.g. "factor_05"
#'
factor_label_to_id <- function(factor_label) {
  as.integer(stringr::str_replace(factor_label, 'factor_', ''))
}


#' Extracts the distance coefficients from the PFMUL output
#'
#' @param data_dir the directory in which the parameter tsv files reside
#' @param shape "matrix" if the raw user x item coefficient matrix should be returned, "long" if the coefficients are to be returned as a tidy (long) data.frame
#'
#' @export
#'
get_distance_coefficients <- function(data_dir, shape = 'long') {
  # read in the item and user loadings on the distance factors
  item_distance_wide <- read_pfmul_parameter_file('beta', 'mean', data_dir, shape = 'wide')
  user_distance_wide <- read_pfmul_parameter_file('gamma', 'mean', data_dir, shape = 'wide')

  # compute the matrix of distance coefficients as the inner product of the item and user loading matrixes
  distance_coefficient_matrix <- as.matrix(user_distance_wide[, c(-1)]) %*% t(as.matrix(item_distance_wide[, c(-1)]))

  if(shape == 'matrix') {
    distance_coefficient_matrix
  } else {
    distance_coefficients <- as.data.frame(distance_coefficient_matrix)
    colnames(distance_coefficients) <- item_distance_wide$item_id
    distance_coefficients$user_id <- user_distance_wide$user_id
    distance_coefficients %>%
      tidyr::gather(item_id, coefficient, -user_id) -> distance_coefficients_long

    distance_coefficients_long
  }
}
