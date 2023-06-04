#' An R wrapper for Jagger's lemmatizer
#'
#' @param input an input.
#' @param model_path  a path to the model.
#' @param keep a vector of POS(s) to keep. Default is `NULL`.
#' @param concat logical. If TRUE, the function returns a concatenated string. Default is `TRUE`.
#' @return a list.
#' @examples
#'  data(sentence_example)
#'  res_lemmatize <- lemmatize(sentence_example$text)
#' @export
lemmatize <- function(input, model_path = NULL, keep = NULL, concat = TRUE) {
  if (is.null(model_path)) {
    model_path <- get_model_path()
  }
  if (!exists_model_path(model_path)) {
    return(input)
  }
  if (! "character" %in% class(input)) {
    cli::cli_abort("Please provide a character vector.")
  }

  if (is.null(keep)) {
    keep_all <- TRUE
    keep <- c("")
  } else {
    keep_all <- FALSE
  }

  lemmatized <- lemmatize_cpp_vec(input, model_path, keep, keep_all)
  if (!concat) {
    lemmatized <- strsplit(lemmatized, " ")
  }
  return(lemmatized)
}

#' An R wrapper for Jagger's lemmatizer (a tibble input)
#'
#' @param tbl a tibble object.
#' @param column a column name of the tibble to tokenize.
#' @param model_path  a path to the model.
#' @param keep a vector of POS(s) to keep. Default is `NULL`.
#' @return a vector.
#' @examples
#'  data(sentence_example)
#'  res_lemmatize <- lemmatize_tbl(tibble::as_tibble(sentence_example), "text")
#' @export
lemmatize_tbl <- function(tbl, column, model_path = NULL, keep = NULL) {
  if (is.null(model_path)) {
    model_path <- get_model_path()
  }
  if (!exists_model_path(model_path)) {
    return(tbl)
  }
  if (! "tbl_df" %in% class(tbl)) {
    cli::cli_abort("Please provide a tibble object.")
  }
  if (! column %in% names(tbl)) {
    cli::cli_abort("Please provide a column name of the tibble.")
  }

  if (is.null(keep)) {
    keep_all <- TRUE
    keep <- c("")
  } else {
    keep_all <- FALSE
  }

  tbl$lemmatized <- lemmatize_cpp_vec(tbl[[column]], model_path, keep, keep_all)
  return(tbl)
}
