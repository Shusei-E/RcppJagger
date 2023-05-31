#' @useDynLib RcppJagger
#' @importFrom Rcpp sourceCpp
NULL

.onUnload <- function(libpath) {
  library.dynam.unload("RcppJagger", libpath)
}