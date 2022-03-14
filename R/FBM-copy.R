################################################################################

#' Copy as a Filebacked Big Matrix
#'
#' Deep copy of a Filebacked Big Matrix with possible subsetting.
#' This should also work for any matrix-like object.
#'
#' @param X Could be any matrix-like object.
#' @inheritParams bigstatsr-package
#' @inheritParams FBM
#'
#' @return A copy of `X` as a new [FBM][FBM-class] object.
#' @export
#'
#' @examples
#' X <- FBM(10, 10, init = 1:100)
#' X[]
#' X2 <- big_copy(X, ind.row = 1:5)
#' X2[]
#'
#' mat <- matrix(101:200, 10)
#' X3 <- big_copy(mat, type = "double")  # as_FBM() would be faster here
#' X3[]
#'
#' X.code <- big_attachExtdata()
#' class(X.code)
#' X2.code <- big_copy(X.code)
#' class(X2.code)
#' all.equal(X.code[], X2.code[])
#'
big_copy <- function(X, ind.row = rows_along(X),
                     ind.col = cols_along(X),
                     type = typeof(X),
                     backingfile = tempfile(tmpdir = getOption("FBM.dir")),
                     block.size = block_size(length(ind.row)),
                     is_read_only = FALSE) {

  if (inherits(X, "FBM.code256") && type == "unsigned char") {
    args <- as.list(environment())
    args$X <- X$as.FBM()
    res <- do.call(big_copy, args)
    return(add_code256(res, code = X$code256))
  }

  res <- FBM(
    nrow = length(ind.row),
    ncol = length(ind.col),
    init = NULL,
    type = type,
    backingfile = backingfile,
    is_read_only = FALSE
  )

  # Don't write in parallel
  big_apply(X, a.FUN = function(X, ind, X2, ind.row, ind.col) {
    X2[, ind] <- X[ind.row, ind.col[ind]]
    NULL
  }, a.combine = 'c', ind = seq_along(ind.col), block.size = block.size,
  X2 = res, ind.row = ind.row, ind.col = ind.col)

  res$is_read_only <- is_read_only
  res
}

################################################################################
