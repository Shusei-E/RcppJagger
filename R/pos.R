#' An R wrapper for Jagger's POS tagger
#'
#' @param input an input.
#' @param model_path  a path to the model.
#' @param keep a vector of POS(s) to keep. Default is `NULL`.
#' @param format a format of the output. Default is `list`.
#' @examples
#' \dontrun{
#'  texts <- read.csv(
#'    "https://raw.githubusercontent.com/koheiw/workshop-IJTA/master/data/asahi.csv",
#'    sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8"
#'  )
#'  pos(texts$head)
#' }
#' @return a list object.
#' @export
pos <- function(input, model_path = NULL, keep = NULL, format = c("list", "data.frame")) {
  if (is.null(model_path)) {
    model_path <- get_model_path()
  }
  if (! "character" %in% class(input)) {
    cli::cli_abort("Please provide a character vector.")
  }
  format <- rlang::arg_match(format)

  result <- pos_cpp_vec(input, model_path)

  if (!is.null(keep)) {
    result <- purrr::map(result, function(x) {
      idx <- x$pos %in% keep
      x$token <- x$token[idx]
      x$lemma <- x$lemma[idx]
      x$subtype <- x$subtype[idx]
      x$pos <- x$pos[idx]
      return(x)
    })
  }

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
#' \dontrun{
#'  texts <- read.csv(
#'    "https://raw.githubusercontent.com/koheiw/workshop-IJTA/master/data/asahi.csv",
#'    sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8"
#'  )
#'  pos_simple(texts$head)
#' }
#' @return a list object.
#' @export
pos_simple <- function(input, model_path = NULL, keep = NULL, format = c("list", "data.frame")) {
  if (is.null(model_path)) {
    model_path <- get_model_path()
  }
  if (! "character" %in% class(input)) {
    cli::cli_abort("Please provide a character vector.")
  }

  format <- rlang::arg_match(format)
  result <- pos_simple_cpp_vec(input, model_path)

  if (!is.null(keep)) {
    result <- purrr::map(result, function(x) {
      idx <- x$pos %in% keep
      x$token <- x$token[idx]
      x$pos <- x$pos[idx]
      return(x)
    })
  }

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
