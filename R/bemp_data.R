#' Read in bemp parameter file
#'
#' @param parameter_name the (character) name of the parameter to be read in, e.g. "alpha" for item factor loadings
#' @param moment the moment of the parameter to be read in, either "mean" or "std"
#' @param model_path the directory in which the results of the BEMP model run reside
#' @param iteration integer: the iteration at which to evaluate the parameters
#' @param shape character: whether to return the parameters in 'long' or 'wide' format
#'
#' @export
read_bemp_parameter_file <- function(parameter_name, moment = 'mean',
                                     model_path, iteration = NULL, shape = 'long') {
  # read in the tsv file
  if(!is.null(iteration)) {
    iteration <- paste0('it', iteration)
  }

  file_name_components <- c('param', parameter_name, iteration, moment)
  file_name <- paste0(paste(file_name_components, collapse = '_'), '.tsv')
  parameter_wide <- data.table::fread(file.path(model_path, file_name))

  # set column names
  unit_id_name <-
    ifelse(parameter_name %in% c('alpha', 'beta'), 'item_id',
    ifelse(parameter_name %in% c('delta'),         'week_id',
                                                   'user_id'))

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

#' create a sequence of factor labels
#'
#' @param length_out length of the sequence
#' @param sequence_type either "robust" (lowercase, underscored and zero-padded) or
#'        "pretty" (to be used in plotting)
#'
#' @export
factor_sequence <- function(length_out, sequence_type = 'robust') {
  if(sequence_type == 'robust') {
    paste('factor',
          stringr::str_pad(seq_len(length_out), 2, pad = '0'),
          sep = '_')
  } else if(sequence_type == 'pretty') {
    paste('Factor',
          seq_len(length_out),
          sep = ' ')
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
#' @param model_path the directory in which the results of the BEMP model run reside
#' @param iteration integer: the iteration at which to evaluate the parameters
#' @param shape "matrix" if the raw user x item coefficient matrix should be returned, "long" if the coefficients are to be returned as a tidy (long) data.frame
#' @export
#'
get_utility_components <- function(component, model_path, iteration = NULL, shape = 'long') {
  if(component == 'latent_factors') {
    item_component_parameter_name <- 'alpha'
    user_component_parameter_name <- 'theta'
  } else if(component == 'distance') {
    item_component_parameter_name <- 'beta'
    user_component_parameter_name <- 'gamma'
  }
  # read in the item and user loadings on the component factors
  item_component_wide <- read_bemp_parameter_file(item_component_parameter_name, 'mean', model_path, iteration, shape = 'wide')
  user_component_wide <- read_bemp_parameter_file(user_component_parameter_name, 'mean', model_path, iteration, shape = 'wide')

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


#' Parse the BEMP logfile
#'
#' @param model_path the directory in which the results of the BEMP model run reside
#' @return nested list
#' @export
#'
parse_bemp_logfile <- function(model_path) {
  ss <- readLines(file.path(model_path, 'log.txt'))

  # reformat and indent the log file so it adheres to yaml format

  # indentation
  ss <- stringr::str_replace_all(ss, '^ \\+', ' - ')
  ss <- stringr::str_replace_all(ss, '^\\t', '      - ')
  ss <- stringr::str_replace_all(ss, '^   -', '    - ')
  ss <- stringr::str_replace_all(ss, '=', ' : ')

  # in yaml a line can either have a value or open a nest but not both. Hack around that
  ss <- stringr::str_replace(ss, 'ICgroups : [0-9]+', 'ICgroups :')
  ICgroups <- stringr::str_match(ss, '    - group ([0-9]+): ([0-9]+-[0-9]+)')
  ss <- ifelse(!is.na(ICgroups[,1]), paste0('    - group_', ICgroups[, 2], ' :\n      - ICidx: ', ICgroups[, 3]), ss)

  # collapse into a single string, then parse using yaml package
  ss <- paste(ss, collapse = '\n')
  parsed <- yaml::yaml.load(ss)

  # FIXME: This isn't quite right. The lowest level of the tree isn't flattened
  parsed %>%
    purrr::modify_depth(.depth = 2, ~purrr::map_if(.x, is.list, purrr::flatten)) %>%
    purrr::modify_depth(.depth = 1, purrr::flatten)
}

#' Get names of the BEMP ICvars
#'
#' @param model_path the directory in which the results of the BEMP model run reside
#' @export
#'
get_icvar_names <- function(model_path) {
  obs_item_path <- file.path(model_path, '..', '..', 'obsItem.tsv')
  obs_item_colnames <- unname(unlist(data.table::fread(obs_item_path,
                                                       nrows = 1,
                                                       header = FALSE)))
  obs_item_colnames[!obs_item_colnames %in% c('location_id')]
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
#' @import data.table
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
#' @param the columns to return. All columns of param_innerProducts.tsv by default but can be narrowed to save on memory
#' @return for each `user_id` - `item_id` combination
#' \itemize{
#'   \item alpha1 = lambda0_i + theta_u * alpha_i + obsItem_u * obsItem_i
#'   \item alpha2 = lambda0_i + theta_u * alpha_i + obsItem_u * obsItem_i + mu_i * delta_w (the average delta_w across all w)
#'   \item eta = gamma_u * beta_i
#' }
#' @import data.table
#' @export
#'
get_bemp_inner_products <- function(model_path, iteration, cols = c('user_id', 'item_id', 'alpha1', 'alpha2', 'eta')) {
  file_name <- file.path(model_path, paste0('param_innerProducts_it', iteration, '.tsv'))

  if(!file.exists(file_name)) {
    stop('No pre-computed inner products exist for that model and that iteration.')
  }

  column_names <- c('user_id', 'item_id', 'alpha1', 'alpha2', 'eta')

  df <- fread(file_name,
              sep = '\t',
              col.names = column_names[column_names %in% cols],
              select = which(column_names %in% cols))

  df
}


#' Get the session data
#'
#' @param model_path the directory in which the raw data reside
#' @param sample "train", "validation", "test", any combination thereof, or "all" as a shorthand
#' @param verbose print messages along the way
#'
#' @import data.table
#' @export
#'
get_sessions <- function(data_dir,
                         samples = c('train', 'test', 'validation'),
                         verbose = FALSE) {
  # expand the above to the session level for all samples that are requested
  valid_samples <- c('train', 'test', 'validation')

  # if "all" samples are requested, replace with all samples
  if(identical(samples, 'all')) {
    samples <- valid_samples
  }

  if (!all(samples %in% valid_samples)) {
    stop('samples must be "train", "validation", "test", any combination thereof, or "all" as a shorthand.')
  }

  if(verbose) { message(paste0('Reading in sessions for ', paste(samples, collapse = ', '), '.')) }
  samples %>%
    purrr::set_names(file.path(data_dir, paste0( . ,'.tsv')), . ) %>%
    purrr::map(data.table::fread) %>%
    data.table::rbindlist(idcol = 'sample') -> obs

  # recast to factor for more efficient storage
  obs[, sample := factor(sample, levels = intersect(valid_samples, samples))]
  data.table::setnames(obs, 'location_id', 'item_id')
  data.table::setnames(obs, 'rating', 'chosen')
  obs[, chosen := as.logical(chosen)]

  obs
}


#' Get the BEMP model internals
#'
#' @param model_path the directory in which the results of the BEMP model run reside
#' @param iteration the iteration
#' @param sample "train", "validation", "test", any combination thereof, or "all" as a shorthand
#' @param cols the columns to return. Choose only those that are necessary for a speedup
#' @param verbose print messages along the way
#' @return data.table containing
#' \itemize{
#'   \item sample
#'   \item session_id
#'   \item user_id
#'   \item item_id
#'   \item alpha1 = lambda0_i + theta_u * alpha_i + obsItem_u * obsItem_i
#'   \item alpha2 = lambda0_i + theta_u * alpha_i + obsItem_u * obsItem_i + mu_i * delta_w (the average delta_w across all w)
#'   \item eta = gamma_u * beta_i
#'   \item distance distance between item and user. For 2017 model vintages this is straight-line distance in km and an extremely large number to make the option unavailable. For 2018 vintages this is some combination of walking and driving time.
#'   \item chosen whether the item was chosen
#'   \item utility utility unter the model: alpha2 - eta * log(distance)
#'   \item choice_prob choice probability under the model
#' }
#' @import data.table
#' @export
#'
get_bemp_model_internals <- function(model_path,
                                     iteration,
                                     samples = c('train', 'test', 'validation'),
                                     cols = c('sample', 'session_id', 'user_id', 'item_id', 'utility'),
                                     verbose = FALSE) {

  # the columns in the inner_products.tsv file. fread will let us skip
  # reading in entire columns if they're not needed so narrow down early
  ip_cols <- c('user_id', 'item_id', 'alpha1', 'alpha2', 'eta')
  get_ip_cols <- intersect(cols, ip_cols)
  if(any(c('choice_prob', 'utility') %in% cols)) {
    get_ip_cols <- c(get_ip_cols, 'alpha2', 'eta')
  }

  if(verbose) { message('Reading in inner products.') }
  # the inner products are at the user x item level
  ip <- get_bemp_inner_products(model_path, iteration, cols = get_ip_cols)

  obs <- get_sessions(file.path(model_path, '..', '..'), samples, verbose = verbose)

  # the above only contain the _chosen_ items in each session. So join in session_ids
  if(verbose) { message('Joining in session_ids') }
  ip <- merge(ip,
              obs[, .(user_id, session_id, sample)],
              by = c('user_id'),
              allow.cartesian = TRUE)
  setkey(ip, session_id)

  # ... then join again to get choices
  if(verbose) { message('Joining in choices.') }
  ip <- merge(ip,
              obs[, .(session_id, item_id, chosen)],
              by = c('session_id', 'item_id'),
              all.x = TRUE)
  ip[, chosen := dplyr::coalesce(chosen, FALSE)]


  # DISTANCES
  # distances are session-specific
  if(any(c('distance', 'utility', 'choice_prob') %in% cols)) {
    if(verbose) { message('Reading in distances.') }
    obs_price <- data.table::fread(file.path(model_path, '..', '..', 'obsPrice.tsv'))
    setnames(obs_price, 'location_id', 'item_id')

    # merge in distances
    if(verbose) { message('Joining in distances.') }
    ip <- merge(ip, obs_price, by = c('session_id', 'item_id'))
  }

  if(any(c('utility', 'choice_prob') %in% cols)) {
    if(verbose) { message('Calculating utilities.') }
    ip[, utility := alpha2 - eta * log(distance)]
  }

  if('choice_prob' %in% cols) {
    if(verbose) { message('Calculating choice probabilities.') }
    ip[, choice_prob := exp(utility) / sum(exp(utility)), .(session_id)]
  }

  # return only the requested columns
  missing_cols <- setdiff(cols, names(ip))
  cols <- intersect(cols, names(ip))
  if (length(missing_cols) > 0){
    warning(paste0("Columns not found: ", paste0(missing_cols, collapse = ", ")))
  }

  ip[, cols, with = FALSE]
}


#' Quickly counts the number of lines in a list of files
#'
#' Uses the unix wc function to count the number of lines in a list of files.
#'
#' @param files a vector listing the files whose line length is desired
#' @param exclude_total when TRUE the combined total line count across all the files is excluded
#' (by default wc returns the length of each file AND the sum of these lengths)
#' @return A vector with the length of each file (and the sum of these lengths if exclude_total is FALSE)
#' @import data.table
#'
count_lines <- function(files, exclude_total = TRUE) {
  system2('wc', args = c('-l', files, " | awk '{print $1}'"), stdout = TRUE) %>%
    as.integer ->
    output

  # By default, if wc is given multiple filess, it also outputs a total line count
  output_length <- length(output)
  if ( output_length > 1 && exclude_total) {
    output <- output[1:(output_length-1)]
  }
  output
}


#' Returns a list of the available checkpoints for a BEMP run
#'
#' @param model_path the directory in which the results of the BEMP model run reside
#' @return a data.table with all existing checkpoints (i.e. innerProduct files)
#' for the selected run, with information from the validation set scores merged on
#' as well as the line count of the innerProduct files, since this can help detect incomplete checkpoints
#' which can occur if a BEMP run is interrupted or crashes.
#' @import data.table
#' @export
#'
get_bemp_checkpoints <- function(model_path) {
  # Get the logs of validation set log likelihoods
  validation_scores <- model_path %>%
    get_bemp_performance_measures %>%
    setDT %>%
    .[dataset=='valid']

  # Get the set of available inner product files
  model_path %>%
    list.files(pattern='param_innerProducts.*', full.names = TRUE) %>%
    data.table(file = .) ->
    inner_product_files

  if (nrow(inner_product_files) == 0) {
    return(NULL)
  } 
  # Check the number of lines in each inner product file so we can exclude partial files
  inner_product_files[, num_rows := count_lines(file)]

  # Identify the iteration number of each file
  inner_product_files[, iteration := stringr::str_extract(file, '(?<=_it)\\d*(?=\\.tsv)') %>% as.integer]
  # Final outputs don't have an iteration number in the file name
  inner_product_files[is.na(iteration), iteration := validation_scores[,max(iteration)]]
  # Merge values from the validation set
  col_names <- c('duration_seconds', 'log_likelihood', 'accuracy', 'precision', 'recall', 'f1score')
  inner_product_files[validation_scores, (col_names) := mget(paste0('i.',col_names)), on='iteration']
  inner_product_files
}
