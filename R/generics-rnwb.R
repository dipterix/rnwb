#' @export
`$.nwb.proxy` <- function(x, name) {
  re <- NextMethod("$")
  if(inherits(re, c(
    "python.builtin.type",
    "python.builtin.function",
    "python.builtin.module"
  ))) {
    cls <- class(re)
    if(!"nwb.proxy" %in% cls) {
      class(re) <- c("nwb.proxy", cls)
    }
    py_names <- attr(re, "py_names")
    attr(re, "py_names") <- c(py_names, name)
  }
  re
}


#' @export
print.nwb.proxy <- function(x, ...) {

  addendums = c()
  doc_string <- tryCatch({
    str <- py_capture_output(
      type = "stdout",
      {
        base <- asNamespace("reticulate")$import_builtins()
        base$help(x)
      }
    )
    str <- trimws(str)
    str <- strsplit(paste(str, collapse = "\n"), "\n")[[1]]
    if(length(str) > 50) {
      str <- c(str[seq_len(50)], "... (Max lines reached, Limit: 50 lines)")

      py_names <- attr(x, "py_names")
      addendums <- c(sprintf("Please use the following command to see the full documentation: {.run rnwb::py_help(%s)}", paste(c("nwb", py_names), collapse = "$")))
    }
    str <- paste(str, collapse = "\n")

    addendums <- c(addendums, "Above documentation is for Python. Please use `$` instead of `.` for modules and functions in R (e.g. nwb$NWBHDF5IO instead of nwb.NWBHDF5IO)")
    str
  }, error = function(e) {
    "*** Cannot retrieve help function..."
  })

  cli::cli_h1("Original python documentation")
  cat(doc_string, "\n", sep = "\n")

  NextMethod()

  if(length(addendums)) {
    cli::cli_h1("Footnotes")

    lapply(seq_along(addendums), function(ii) {
      cli::cli_alert_info(addendums[[ii]])
    })

  }
  cli::cli_rule()
  invisible(x)
}
