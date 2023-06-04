get_model_path <- function() {
  if (Sys.info()["sysname"] == "Darwin") {
    return("/usr/local/lib/jagger/model/kwdlc")
  } else {
    return(NULL)
  }
}

exists_model_path <- function(path) {
  if (is.null(path)) {
    return(FALSE)
  } else {
    return(dir.exists(path))
  }
}
