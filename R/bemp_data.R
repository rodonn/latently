#' Read in bemp parameter file
#'
#' @param parameter_name the (character) name of the parameter to be read in, e.g. "alpha" for item factor loadings
#' @param moment the moment of the parameter to be read in, either "mean" or "std"
#' @param data_dir the directory in which the parameter tsv files reside
#' @param iteration integer: the iteration at which to evaluate the parameters
#' @param shape character: whether to return the parameters in 'long' or 'wide' format
#'
#' @export
read_bemp_parameter_file <- function(parameter_name, moment = 'mean',
                                      data_dir, iteration = NULL, shape = 'long') {
  # read in the tsv file
  if(!is.null(iteration)) {
    iteration <- paste0('it', iteration)
  }

  file_name_components <- c('param', parameter_name, iteration, moment)
  file_name <- paste0(paste(file_name_components, collapse = '_'), '.tsv')
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


#' Extract utility components from the bemp output
#'
#' @param component character: either "latent_factors" or "distance"
#' @param data_dir the directory in which the parameter tsv files reside
#' @param iteration integer: the iteration at which to evaluate the parameters
#' @param shape "matrix" if the raw user x item coefficient matrix should be returned, "long" if the coefficients are to be returned as a tidy (long) data.frame
#' @export
#'
get_utility_components <- function(component, data_dir, iteration = NULL, shape = 'long') {
  if(component == 'latent_factors') {
    item_component_parameter_name <- 'alpha'
    user_component_parameter_name <- 'theta'
  } else if(component == 'distance') {
    item_component_parameter_name <- 'beta'
    user_component_parameter_name <- 'gamma'
  }
  # read in the item and user loadings on the component factors
  item_component_wide <- read_bemp_parameter_file(item_component_parameter_name, 'mean', data_dir, iteration, shape = 'wide')
  user_component_wide <- read_bemp_parameter_file(user_component_parameter_name, 'mean', data_dir, iteration, shape = 'wide')

  # compute the matrix of utility coefficients as the inner product of the item and user loading matrixes
  utility_coefficient_matrix <- as.matrix(user_component_wide[, c(-1)]) %*% t(as.matrix(item_component_wide[, c(-1)]))

  if(shape == 'matrix') {
    utility_coefficient_matrix
  } else {
    utility_coefficients <- as.data.frame(utility_coefficient_matrix)
    colnames(utility_coefficients) <- item_component_wide$item_id
    utility_coefficients$user_id <- user_component_wide$user_id
    utility_coefficients %>%
      tidyr::gather(item_id, coefficient, -user_id) %>%
      mutate(item_id = as.integer(item_id)) -> utility_coefficients_long

    utility_coefficients_long
  }
}
