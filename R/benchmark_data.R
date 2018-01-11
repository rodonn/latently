#' Get the Stata model internals
#'
#' @param predictions_file_path the path to the predictions TSV file the Stata model outputs
#' @param input_data_path the directory in which the BEMP input data resides
#' @param sample "train", "validation", "test", any combination thereof, or "all" as a shorthand
#' @param cols the columns to return. Choose only those that are necessary for a speedup
#' @param verbose print messages along the way
#' @return data.table containing
#' \itemize{
#'   \item sample
#'   \item session_id
#'   \item user_id
#'   \item item_id
#'   \item chosen whether the item was chosen
#'   \item utility utility unter the model
#'   \item choice_prob choice probability under the model
#' }
#' @import data.table
#' @export
#'
get_stata_model_internals <- function(predictions_file_path,
                                      input_data_path,
                                      samples = c('train', 'test', 'validation'),
                                      cols = c('sample', 'session_id', 'user_id', 'item_id', 'utility'),
                                      verbose = FALSE) {
  if(verbose) { message('Reading in predictions tsv file') }

  # the predictions at the session level
  ip <- data.table::fread(predictions_file_path,
                          sep = '\t',
                          colClasses = c('integer', 'integer', 'integer', 'numeric', 'numeric'))
  setkey(ip, session_id)
  # harmonize naming with BEMP output
  setnames(ip, c('pHat', 'util'), c('choice_prob', 'utility'))

  # Join in which sample each of the sessions belongs to
  if(verbose) { message('Joining in sample information') }
  obs <- get_sessions(input_data_path, samples, verbose = verbose)
  ip <- merge(ip,
              obs[, .(session_id, sample)],
              by = c('session_id'),
              all.x = TRUE,
              allow.cartesian = FALSE)

  if('chosen' %in% cols) {
    # join the chosen items into each session
    if(verbose) { message('Joining in choices') }

    ip <- merge(ip,
                obs[, .(session_id, item_id, chosen)],
                by = c('session_id', 'item_id'),
                all.x = TRUE)
    ip[, chosen := dplyr::coalesce(chosen, FALSE)]
  }

  # DISTANCES
  # distances are session-specific
  if('distance' %in% cols) {
    if(verbose) { message('Reading in distances.') }
    obs_price <- data.table::fread(file.path(input_data_path, 'obsPrice.tsv'))
    setnames(obs_price, 'location_id', 'item_id')

    # merge in distances
    if(verbose) { message('Joining in distances.') }
    ip <- merge(ip,
                obs_price,
                by = c('session_id', 'item_id'),
                all.x = TRUE)
  }

  # return only the requested columns
  missing_cols <- setdiff(cols, names(ip))
  cols <- intersect(cols, names(ip))
  if (length(missing_cols) > 0){
    warning(paste0("Columns not found: ", paste0(missing_cols, collapse = ", ")))
  }

  ip[, cols, with = FALSE]
}


#' Gets predictions from bemp, stata, or other models
#'
#' @param model_type Can be bemp, stata, or anything else (as long as it has a predictions.tsv file)
#' @param model_path The folder containing the outputs from the model
#' @param input_data_path The folder that contains the bemp-style model inputs
#' @param selected_iteration For bemp models only. Which checkpoint to use.
#' @param verbose 
#' @return A data.table with the predictions from the model
#' @import data.table
#' @export
#'
get_model_predictions <- function(model_type,
                                  model_path,
                                  input_data_path,
                                  selected_iteration = 0,
                                  verbose = FALSE) {
  if (model_type == 'bemp') {
    selected_cols <- c('user_id','session_id','item_id','sample',
                       'distance','chosen','utility','choice_prob','eta','alpha2')
    predictions <- get_bemp_model_internals(model_path,
                                            iteration = selected_iteration,
                                            cols = selected_cols,
                                            verbose = verbose)
  } else if (model_type == 'stata') {
    selected_cols <- c('user_id','session_id','item_id','sample',
                       'distance','chosen','utility','choice_prob','eta')

    predictions_file <- list.files(model_path, pattern = 'predictions.*\\.tsv', full.names=TRUE)
    if (length(predictions_file) > 1) {
      predictions_file <- predictions_file[1]
      stop('Multiple predictions files detected, using ', predictions_file)
    }

    predictions <- get_stata_model_internals(predictions_file,
                                             input_data_path,
                                             samples = c('train','validation','test'),
                                             cols = selected_cols,
                                             verbose = verbose)
  } else {
    ## Load any other model type's predictions, as long as it has a predictions.tsv file
    prediction_path <- file.path(model_path, 'predictions.tsv')
    if (file.exists(prediction_path)){
      predictions <- fread(prediction_path)
    } else {
      stop(paste0('Could not find predictions.tsv file in ', model_path))
    }
  }

  predictions
}

