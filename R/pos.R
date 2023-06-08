#' An R wrapper for Jagger's POS tagger
#'
#' @param input an input.
#' @param model_path  a path to the model.
#' @param keep a vector of POS(s) to keep. Default is `NULL`.
#' @param format a format of the output. Default is `list`.
#' @examples
#'  data(sentence_example)
#'  res_pos <- pos(sentence_example$text)
#' @return a list object.
#' @export
pos <- function(input, model_path = NULL, keep = NULL, format = c("list", "data.frame")) {
  if (is.null(model_path)) {
    model_path <- get_model_path()
  }
  if (!exists_model_path(model_path)) {
    return(input)
  }
  if (! "character" %in% class(input)) {
    cli::cli_abort("Please provide a character vector.")
  }
  format <- rlang::arg_match(format)

  if (is.null(keep)) {
    keep_all <- TRUE
    keep <- c("")
  } else {
    keep_all <- FALSE
  }

  result <- pos_cpp_vec(input, model_path, keep, keep_all)

  if (format == "data.frame") {
    result <- purrr::map(result, function(x) {
      return(data.frame(
        token = x$token,
        pos = x$pos,
        subtype = x$subtype,
        lemma = x$lemma
      ))
    })
  }
  return(result)
}

#' An R wrapper for Jagger's POS tagger (only returning POS)
#'
#' @param input an input.
#' @param model_path  a path to the model.
#' @param keep a vector of POS(s) to keep. Default is `NULL`.
#' @param format a format of the output. Default is `list`.
#' @examples
#'  data(sentence_example)
#'  res_pos <- pos_simple(sentence_example$text)
#' @return a list object.
#' @export
pos_simple <- function(input, model_path = NULL, keep = NULL, format = c("list", "data.frame")) {
  if (is.null(model_path)) {
    model_path <- get_model_path()
  }
  if (!exists_model_path(model_path)) {
    return(input)
  }
  if (! "character" %in% class(input)) {
    cli::cli_abort("Please provide a character vector.")
  }

  format <- rlang::arg_match(format)

  if (is.null(keep)) {
    keep_all <- TRUE
    keep <- c("")
  } else {
    keep_all <- FALSE
  }

  result <- pos_simple_cpp_vec(input, model_path, keep, keep_all)

  if (format == "data.frame") {
    result <- purrr::map(result, function(x) {
      return(data.frame(
        token = x$token,
        pos = x$pos
      ))
    })
  }
  return(result)
}
