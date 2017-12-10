#' Read in bemp parameter file
#'
#' @param parameter_name the (character) name of the parameter to be read in, e.g. "alpha" for item factor loadings
#' @param moment the moment of the parameter to be read in, either "mean" or "std"
#' @param data_dir the directory in which the results of the BEMP model run reside
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
#' @param data_dir the directory in which the results of the BEMP model run reside
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

#' Parse the options the bemp model was estimated with given a directory name
#'
#' @param label the label of the bemp model, often the directory name under which it is saved
#' @return tibble with parameter:value pairs
#' @export
#'
parse_bemp_label <- function(description) {
  tibble(parameter_keyvals = description) %>%
    tidyr::separate_rows(parameter_keyvals, sep = '-') %>%
    mutate(value = stringi::stri_reverse(stringr::str_match(stringi::stri_reverse(parameter_keyvals), '(^[0-9\\.]+)')[, 2])) %>%
    mutate(parameter = stringi::stri_reverse(stringr::str_replace(stringi::stri_reverse(parameter_keyvals), '(^[0-9\\.]+)', ''))) %>%
    select(parameter, value)
}

#' Get BEMP performance measures
#'
#' @param model_path the directory in which the results of the BEMP model run reside
#' @return a tibble with the following columns:
#' \itemize{
#'   \item iteration
#'   \item duration_seconds
#'   \item log_likelihood
#'   \item accuracy: the fraction of correctly classified instances.
#'   \item \eqn{precision_i}: the fraction of instances where we correctly declared \eqn{i} out of all instances where the algorithm declared \eqn{i} (then I take the average across all \eqn{i})
#'   \item recall: the fraction of instances where we correctly declared \eqn{i} out of all of the cases where the true of choice was \eqn{i} (also averaged across all \eqn{i})
#'   \item F1-score: defined according to the formula \eqn{f1_score = 2 * precision * recall / (precision + recall)}
#'   \item total_instances
#' }
#' @export
#'
get_bemp_performance_measures <- function(model_path) {
  datasets <- c('train', 'test', 'valid')

  datasets %>%
    purrr::map_dfr(~fread(file.path(model_path, paste(.x, 'tsv', sep = '.')),
                          sep = '\t',
                          col.names = c('iteration', 'duration_seconds',
                                        'log_likelihood', 'accuracy',
                                        'precision', 'recall',
                                        'f1score', 'total_instances')) %>%
                     mutate(dataset = .x))
}


#' Get pre-computed inner products for the BEMP model
#'
#' @param model_path the directory in which the results of the BEMP model run reside
#' @param iteration the iteration
#' @return for each `user_id` - `item_id` combination
#' \itemize{
#'   \item alpha1 = lambda0_i + theta_u * alpha_i + obsItem_u * obsItem_i
#'   \item alpha2 = lambda0_i + theta_u * alpha_i + obsItem_u * obsItem_i + mu_i * delta_w (the average delta_w across all w)
#'   \item eta = gamma_u * beta_i
#' }
#' @export
#'
get_bemp_inner_products <- function(model_path, iteration) {
  file_name <- file.path(model_path, paste0('param_innerProducts_it', iteration))

  if(!file.exists(file_name)) {
    stop('No pre-computed inner products exist for that model and that iteration.')
  }

  df <- fread(file_name,
              sep = '\t',
              col.names = c('user_id', 'item_id', 'alpha1', 'alpha2', 'eta'))

  df
}

#' Get the BEMP model internals
#'
#' @param model_path the directory in which the results of the BEMP model run reside
#' @param iteration the iteration
#' @return data.table containing
#' \itemize{
#'   \item session_id
#'   \item user_id
#'   \item item_id
#'   \item alpha1 = lambda0_i + theta_u * alpha_i + obsItem_u * obsItem_i
#'   \item alpha2 = lambda0_i + theta_u * alpha_i + obsItem_u * obsItem_i + mu_i * delta_w (the average delta_w across all w)
#'   \item eta = gamma_u * beta_i
#'   \item distance distance between item and user in km
#'   \item chosen whether the item was chosen
#'   \item utility utility unter the model: alpha2 - eta * log(distance)
#'   \item choice_prob choice probability under the model
#' }
#' @export
#'
get_bemp_model_internals <- function(model_path, iteration) {
  ip <- get_bemp_inner_products(model_path, iteration)

  train <- data.table::fread(file.path(model_path, '..', '..', 'train.tsv'))
  setnames(train, 'location_id', 'item_id')

  obs_price <- data.table::fread(file.path(model_path, '..', '..', 'obsPrice.tsv'))
  setnames(obs_price, 'location_id', 'item_id')

  # merge the user_ids for each session
  obs_price_train <- merge(obs_price,
                           train[, .(user_id, session_id)],
                           by = 'session_id')

  # merge in actual choices (mostly for debugging)
  obs_price_train <- merge(obs_price_train,
                           train[, .(session_id, item_id, rating)],
                           by = c('session_id', 'item_id'), all.x = TRUE)
  obs_price_train[is.na(rating), rating := 0]
  obs_price_train[, rating := as.logical(rating)]

  ip <- merge(ip,
              obs_price_train,
              by = c('user_id', 'item_id'))
  setkey(ip, session_id)

  ip[, utility := alpha2 - eta * log(distance)]
  ip[, choice_prob := exp(utility) / sum(exp(utility)), .(session_id)]

  setcolorder(ip, c('session_id', 'user_id', 'item_id', 'alpha1', 'alpha2',
                    'eta', 'distance', 'chosen = rating', 'utility', 'choice_prob'))

  ip
}
