# This script will run at the end of package loading phase

.pynwb <- local({

  pynwb <- NULL

  get_pynwb <- function(force = FALSE, error_if_missing = TRUE) {
    if(!force && inherits(pynwb, "python.builtin.module")) {
      return( pynwb )
    }
    if( !rpymat_is_setup() ) {
      if( error_if_missing ) {
        stop("Please configure environment first. Run the following command:\n   rnwb::install_nwb()")
      }
      return( NULL )
    }
    tryCatch({
      rpymat::ensure_rpymat(verbose = FALSE)
      m <- rpymat::import("pynwb", convert = FALSE, delay_load = FALSE, as = "pynwb")
      class(m) <- c('nwb.proxy', class(m))
      pynwb <<- m
      return( pynwb )
    }, error = function(e) {
      if( error_if_missing ) {
        stop(e)
      }
      return(NULL)
    })
  }

  clean_pynwb <- function() {
    pynwb <<- NULL
  }

  list(
    get = get_pynwb,
    clean = clean_pynwb
  )
})


load_py <- local({

  main <- NULL

  function() {
    if (!is.null(main)) { return(main) }

    if( !rpymat_is_setup() ) {
      return( NULL )
    }

    py <- tryCatch({
      reticulate <- asNamespace("reticulate")
      if(isTRUE(reticulate$is_python_initialized())) {
        py <- reticulate$import_main(convert = TRUE)
      } else {
        py <- NULL
      }
      py
    }, error = function(e) {
      reticulate$py
    })

    if(!is.null(py)) {
      main <<- py
    }
    main
  }
})

# inject python and
.onLoad <- function(libname, pkgname) {
  pkg <- getNamespace(pkgname)
  makeActiveBinding("py", fun = load_py, env = pkg)
  makeActiveBinding(
    "nwb", env = pkg,
    fun = function() {
      load_nwb(error_if_missing = FALSE)
    }
  )
}

.onUnload <- function(libpath) {
  .pynwb$clean()
}

#' @name nwb
#' @title Get 'PyNWB' module
#' @param force whether to force reloading \code{PyNWB} module; default is false
#' @param error_if_missing whether to raise errors when the module is unable to
#' load; default is true.
#' @usage nwb
#' @returns A 'Python' module if successfully loaded. If \code{error_if_missing}
#' is set to false and module is unable to load, return \code{NULL}
#' @export
NULL

#' @rdname nwb
#' @export
load_nwb <- .pynwb$get

#' @name py
#' @title Get 'Python' main process environment
#' @usage py
#' @returns The 'Python' main process as a module
#' @export
"py"
NULL
