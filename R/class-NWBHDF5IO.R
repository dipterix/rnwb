#' @export
NWBHDF5IO <- R6::R6Class(
  classname = "NWBHDF5IO",
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    construct_params = NULL,
    .file_handler = NULL,
    ensure_file_handler = function() {
      # check if .file_handler is valid
      ptr <- private$.file_handler
      if(is.null(ptr) || is_invalid_pointer(ptr)) {
        nwb <- load_nwb()
        ptr <- do.call(nwb$NWBHDF5IO, private$construct_params)
        private$.file_handler <- ptr
      }
      return(ptr)
    },
    finalize = function(...) {
      self$close()
    }
  ),
  public = list(
    initialize = function(
      path = NULL, mode = c("r", "w", "r+", "a", "w-", "x"), ...) {
      if(length(path)) {
        path <- normalizePath(path)
      }
      mode <- match.arg(mode)
      private$construct_params <- list(
        path = path, mode = mode, ...
      )
    },
    close = function(close_links = TRUE) {
      ptr <- private$.file_handler
      if(is.null(ptr) || is_invalid_pointer(ptr)) { return(invisible()) }
      # clean up and close the file handler
      verbose_debug("Closing the [NWBHDF5IO] file handler")
      tryCatch({
        ptr$close(close_links = as.logical(close_links))
      }, error = warning)
      # important: remove the pointer or segfault
      private$.file_handler <- NULL
      invisible()
    },
    close_linked_files = function() {
      ptr <- private$.file_handler
      if(is.null(ptr) || is_invalid_pointer(ptr)) { return(invisible()) }
      # clean up and close the file handler
      verbose_debug("Closing the [NWBHDF5IO] all linked files")
      tryCatch({
        ptr$close_linked_files()
      }, error = warning)
      # important: remove the pointer or segfault
      private$.file_handler <- NULL
      invisible()
    },
    read = function() {
      if(!self$opened) {
        stop("The file is not opened. Must run under x$with context")
      }
      handler <- private$ensure_file_handler()
      return(handler$read())
    },
    with = function(expr, quoted = FALSE, envir = parent.frame()) {
      if(!quoted) {
        expr <- substitute(expr)
      }
      private$ensure_file_handler()
      # Make sure closing the file links when exiting the function
      on.exit({ self$close() })
      if(!self$opened) {
        stop("Cannot open file...")
      }
      eval(expr, envir = envir)
    }
  ),
  active = list(
    opened = function() {
      ptr <- private$.file_handler
      if(is.null(ptr) || is_invalid_pointer(ptr)) {
        return(FALSE)
      }
      return(TRUE)
    }
  )
)
