# Util functions that may be used repeatedly

is_invalid_pointer <- function(x) {
  rpymat::ensure_rpymat(verbose = FALSE)
  asNamespace("reticulate")$py_is_null_xptr(x)
}

verbose_debug <- function(...) {
  if(getOption("rnwb.debug", FALSE)) {
    message("[rnwb.debug] ", ...)
  }
}

#' Try to convert an object to an R object
#' @param x input data, can be either an 'R' or 'Python' object
#' @param on_fail what to do when the object is still a 'Python' object;
#' default is to \code{'ignore'}, other choices are \code{'error'}
#' and \code{'warning'}
#' @returns The converted R object
#' @export
to_r <- function(x, on_fail = c("ignore", "error", "warning")) {
  on_fail <- match.arg(on_fail)
  if(inherits(x, "python.builtin.object")) {
    x <- rpymat::py_to_r(x)
    if(on_fail != "ignore" && inherits(x, "python.builtin.object")) {
      msg <- paste(c(
        "`to_r`: Unable to convert the following object to R:",
        format(x)
      ), collapse = "\n")
      if(on_fail == "error") {
        stop(msg)
      } else {
        warning(msg)
      }
    }
  }
  return(x)
}
