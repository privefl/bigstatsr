################################################################################

#' @importFrom bigassertr printf message2 warning2 stop2

################################################################################

as_vec <- function(x) {
  x2 <- drop(x)
  if (is.matrix(x2))
    stop2("'%s' must a vector, not a matrix.", deparse(substitute(x)))
  x2
}

################################################################################

#' Temporarily disable downcast warning
#'
#' @param expr The expression to evaluate without downcast warning.
#'
#' @return The result of the evaluated expression.
#' @export
#'
#' @examples
#' without_downcast_warning(FBM(10, 10, type = "integer", init = 1.5))
without_downcast_warning <- function(expr) {

  opt.save <- options(bigstatsr.downcast.warning = FALSE)
  on.exit(options(opt.save), add = TRUE)

  eval.parent(substitute(expr))
}

################################################################################

#' @importFrom bigassertr assert_args assert_nodots
#' @importFrom bigassertr assert_int assert_pos assert_all assert_nona
#' @importFrom bigassertr assert_01 assert_multiple assert_lengths
#' @importFrom bigassertr assert_class assert_class_or_null
#' @importFrom bigassertr assert_dir assert_exist assert_noexist
#' @importFrom bigparallelr assert_cores

################################################################################

check_args <- function(...) {

  if (getOption("bigstatsr.check.args")) {
    args <- as.list(parent.frame())

    check <- c(
      list(...),  # possible to "overwrite" following defaults
      list(
        X            = "assert_class(X, 'FBM'); assert_noNA(X)",
        X.code       =
          "assert_class(X.code, 'FBM.code256'); assert_noNA(X.code)",
        y.train      = "assert_nona(y.train); assert_multiple(y.train)",
        y01.train    = "assert_01(y01.train)",
        ind.train    = "assert_int(ind.train); assert_pos(ind.train)",
        ind.row      = "assert_int(ind.row);   assert_pos(ind.row)",
        ind.col      = "assert_int(ind.col);   assert_pos(ind.col)",
        ncores       = "assert_cores(ncores)",
        fun.scaling  = "assert_args(fun.scaling, c('ind.row', 'ind.col'))",
        covar.train  =
          "assert_class_or_null(covar.train, 'matrix'); assert_nona(covar.train)",
        covar.row    =
          "assert_class_or_null(covar.row,   'matrix'); assert_nona(covar.row)"
      )
    )

    for (i in match(names(args), names(check)))
      if (!is.na(i)) with(args, eval(parse(text = check[i])))
  }
}

################################################################################

# MISSING VALUES
assert_noNA <- function(x) {
  ind <- outer(0:100, sample(length(x), 5, replace = TRUE), "+")
  ind <- sort(pmin(ind, length(x)))
  if (anyNA(x[ind]))
    stop2("You can't have missing values in '%s'.", deparse(substitute(x)))
}

################################################################################
