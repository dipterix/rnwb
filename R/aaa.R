#' @importFrom R6 R6Class
#' @importFrom cli cli_h1
#' @import rpymat
NULL

# Internals

rpymat_is_setup <- function() {
  return( dir.exists(rpymat::env_path()) )
}

py_capture_output <- function(...) {
  rpymat::ensure_rpymat(verbose = FALSE)
  asNamespace("reticulate")$py_capture_output(...)
}

# Installs
validate_python <- function(verbose = TRUE) {
  verb <- function(expr) {
    if(verbose) {
      force( expr )
    }
  }
  verb(message("Initializing python environment: "))

  rpymat::ensure_rpymat(verbose = verbose)

  verb(message("Trying to get installed packages..."))
  tbl <- rpymat::list_pkgs()
  pkgs <- tbl$package
  pkgs <- pkgs[grepl("^[a-zA-Z0-9]", pkgs)]

  verb(cat("Installed packages:", paste(pkgs, collapse = ", "), "\n"))

  # Check environment
  verb(message("Trying to validate packages..."))

  package_missing <- NULL
  for(package in c("pynwb")) {
    tryCatch({
      verb({ cat(sprintf("%s: ...", package)) })
      module <- rpymat::import(package)
      verb({ cat("\b\b\b", module$`__version__`, "\n", sep = "") })
    }, error = function(e) {
      verb({ cat("\b\b\bN/A\n", sep = "") })
      package_missing <<- c(package_missing, package)
    })
  }

  return(invisible(package_missing))
}

#' @title Install 'NWB' via 'PyNWB'
#' @param python_ver 'Python' version, see \code{\link[rpymat]{configure_conda}};
#' default is \code{"auto"}, which is suggested
#' @param verbose whether to print the installation messages
#' @returns This function returns nothing.
#' @export
install_nwb <- function(python_ver = "auto", verbose = TRUE) {
  if(!dir.exists(rpymat::env_path())) {
    standalone <- !file.exists(rpymat::conda_bin())
    rpymat::configure_conda(python_ver = python_ver, force = TRUE, standalone = standalone)
  }
  rpymat::ensure_rpymat(verbose = verbose)
  installed_pkgs_tbl <- rpymat::list_pkgs()

  # install necessary libraries
  pkgs <- c("pynwb")
  if(!all(pkgs %in% installed_pkgs_tbl$package)) {
    rpymat::add_packages(pkgs)
  }

  validate_python(verbose = verbose)
  return(invisible())
}


