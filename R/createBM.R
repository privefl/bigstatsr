################################################################################

#' Functions for creating a "big.matrix"
#'
#' The 3 following functions return **functions** as well:
#' - `BM`: wrapper around [big.matrix].
#'
#' @inheritDotParams bigmemory::big.matrix -nrow -ncol -type
#'
#' @return A **function** of 3 parameters `nrow`, `ncol` and `type` (default is
#' `"double"`) that is used to create a `big.matrix`.
#' This is particularly useful as the `fun.createBM` parameter.
#'
#' @export
#'
#' @rdname createBM
#'
#' @examples
#' # these functions return **functions**
#' (f <- BM(shared = FALSE, descriptor = FALSE))
#' # with 3 parameters
#' (X <- f(nrow = 10, ncol = 6, type = "integer"))
#' is.shared(X)
#'
#' # a shortcut for a filebacked `big.matrix`
#' X2.desc <- FBM(backingroot = "test", init = 2)(10, 10)
#' str(X2.desc)
#' attach.BM(X2.desc)[,]
#' file.remove(paste0("backingfiles/test", c(".bk", ".desc")))
#'
#' # a temporary filebacked `big.matrix`
#' test <- tmpFBM()(10, 10)
#' str(test)
BM <- function(..., descriptor = TRUE) {
  function(nrow, ncol, type = "double") {
    res <- big.matrix(nrow, ncol, type, ...)
    `if`(descriptor, describe(res), res)
  }
}

################################################################################

#' Filebacked "big.matrix"
#'
#' - `FBM`: wrapper around [filebacked.big.matrix]. In particular:
#'   - backingpath is `"backingfiles"` by default,
#'   - extension `.bk` is used,
#'   - descriptor is never binary and has an extension ".desc",
#'   - the `big.matrix` has no dimname,
#'   - option `separated` is never used.
#'
#' @param init A scalar value for initializing the matrix
#'   (`NULL` by default to avoid unnecessary time spent doing the initializing).
#' @param backingroot The root of the backing file to be created.
#'   This should not be a full path (use `backingpath`)
#'   nor have any extension (".bk" is used by default).
#' @param backingpath The directory of the backing file to be created.
#'   Default is `"backingfiles"`. The directory is created if doesn't exist.
#' @param descriptor Return the descriptor of the resulting `big.matrix` or
#'   directly the `big.matrix` object? Default is `TRUE` (the descriptor).
#'
#' @export
#' @rdname createBM
#'
FBM <- function(backingroot,
                backingpath = "backingfiles",
                init = NULL,
                descriptor = TRUE) {

  function(nrow, ncol, type = "double") {

    # create directory if doesn't exist
    assert_dir(backingpath)

    # Wrapper around `filebacked.big.matrix`
    res <- filebacked.big.matrix(
      nrow = nrow,
      ncol = ncol,
      type = type,
      init = init,
      dimnames = NULL,
      separated = FALSE,
      backingfile = paste0(backingroot, ".bk"),
      backingpath = backingpath,
      descriptorfile = paste0(backingroot, ".desc"),
      binarydescriptor = FALSE
    )

    # return
    `if`(descriptor, describe(res), res)
  }
}

################################################################################

#' Temporary filebacked "big.matrix"
#'
#' - `tmpFBM`: wrapper around `FBM` to get a temporary filebacked `big.matrix`.
#' It uses [tempfile] to get the location of
#' the temporary filebacked `big.matrix`.
#' Temporary means not remaining if you restart your R session.
#'
#' @export
#' @rdname createBM
#'
tmpFBM <- function(init = NULL, descriptor = TRUE) {

  tmpfile <- tempfile()

  FBM(backingroot = basename(tmpfile),
      backingpath = dirname(tmpfile),
      init = init,
      descriptor = descriptor)
}

################################################################################

BM.path <- function(X.) {
  X <- attach.BM(X.)
  file.path(dir.name(X), file.name(X))
}

################################################################################

#' Remove temporary backing files
#'
#' @inheritParams bigstatsr-package
#'
#' @inherit base::file.remove return
#' @export
#'
#' @examples
#' list.files(path = tempdir())
#' X.desc <- tmpFBM()(10, 10)
#' list.files(path = tempdir())
#'
#' tmpFBM.rm(X.desc)
#' list.files(path = tempdir())
#'
tmpFBM.rm <- function(X.) {

  path <- BM.path(X.)

  if (startsWith(normalizePath(path), tempdir())) {
    file.remove(path, sub("\\.bk$", ".desc", path))
  } else {
    warning2("Is '%s' really a temporary FBM? Aborting..", path)
  }
}

################################################################################
