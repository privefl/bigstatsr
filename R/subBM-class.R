################################################################################

#' S3 class subBM
#'
#' A class for representing a subset view of a `big.matrix`.
#'
#' A named list with at 3 slots: \describe{
#'   \item{desc}{The `big.matrix.descriptor` object.}
#'   \item{rows}{Indices of rows to be used.}
#'   \item{cols}{Indices of columns to be used.}
#' }
#'
#' @name subBM-class
#' @aliases subBM-class subBM
NULL

################################################################################



################################################################################

## Dimensions

#' @rdname subBM-class
#' @S3method subBM nrow
nrow.subBM <- function(x) length(x$rows)

#' @rdname subBM-class
#' @export
ncol.subBM <- function(x) length(x$cols)

#' @rdname subBM-class
#' @export
dim.subBM <- function(x) c(nrow(x), ncol(x))

#' @rdname subBM-class
#' @export
length.subBM <- function(x) prod(dim(x))

################################################################################
