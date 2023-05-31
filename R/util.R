get_model_path <- function() {
  if (Sys.info()["sysname"] == "Darwin") {
    return("/usr/local/lib/jagger/model/kwdlc")
  } else {
    cli::cli_abort("Please manually specify the {.var model_path} argument.")
  }
}