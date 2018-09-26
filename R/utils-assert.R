################################################################################

printf <- function(...) cat(sprintf(...))
message2 <- function(...) message(sprintf(...))
warning2 <- function(...) warning(sprintf(...), call. = FALSE)
stop2 <- function(...) stop(sprintf(...), call. = FALSE)

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

check_args <- function(...) {

  if (getOption("bigstatsr.check.args")) {
    args <- as.list(parent.frame())

    check <- c(
      list(...),  # possible to "overwrite" following defaults
      list(
        X            = "assert_class(X, 'FBM'); assert_noNA(X)",
        X.code       =
          "assert_class(X.code, 'FBM.code256'); assert_noNA(X.code)",
        y01.train    = "assert_01(y01.train)",
        ind.train    = "assert_int(ind.train); assert_pos(ind.train)",
        ind.row      = "assert_int(ind.row);   assert_pos(ind.row)",
        ind.col      = "assert_int(ind.col);   assert_pos(ind.col)",
        ncores       = "assert_cores(ncores)",
        fun.scaling  = "assert_args(fun.scaling, c('ind.row', 'ind.col'))",
        covar.train  =
          "if (!is.null(covar.train)) assert_class(covar.train, 'matrix')",
        covar.row    =
          "if (!is.null(covar.row)) assert_class(covar.row, 'matrix')"
      )
    )

    for (i in match(names(args), names(check)))
      if (!is.na(i)) with(args, eval(parse(text = check[i])))
  }
}

################################################################################

# MISSING VALUES
assert_noNA <- function(x) {
  lim <- min(1000, length(x))
  if (sum(is.na(x[seq_len(lim)])))
    stop2("You can't have missing values in '%s'.", deparse(substitute(x)))
}

################################################################################

# ARGS
assert_args <- function(f, args.name) {

  if (!inherits(f, "function"))
    stop2("'%s' is not a function.", deparse(substitute(f)))

  if (!all(args.name %in% names(formals(f))))
    stop2("'%s' should have argument%s named %s.",
          deparse(substitute(f)),
          `if`(length(args.name) > 1, "s", ""),
          toString(args.name))
}

################################################################################

# NUMBER OF CORES
assert_cores <- function(ncores) {
  if (ncores > getOption("bigstatsr.ncores.max")) {
    stop2(paste0("You are trying to use more cores than allowed.",
                 " We advise you to use `nb_cores()`.\n",
                 "You can change this default value with",
                 " `options(bigstatsr.ncores.max = Inf)`."))
  }
}

################################################################################

assert_lengths <- function(...) {
  lengths <- lengths(list(...))
  if (length(lengths) > 1) {
    if (any(diff(lengths) != 0))
      stop2(GET_ERROR_DIM())
  } else {
    stop2("You should check the lengths of at least two elements.")
  }
}

################################################################################

# INTEGERS
assert_int <- function(x) {
  if (!is.null(x)) {
    var_name <- deparse(substitute(x))
    is_int <- tryCatch(all(x == trunc(x)), error = function(e) {
      stop2("'%s' should be numeric.", var_name)
    })
    if (!is_int) stop2("'%s' should contain only integers.", var_name)
  }
}

################################################################################

# POSITIVE INDICES
assert_pos <- function(x)  {
  if (!all(x > 0))
    stop2("'%s' should have only positive values.", deparse(substitute(x)))
}

################################################################################

# 0s AND 1s
assert_01 <- function(x)  {
  if (!all(x %in% 0:1))
    stop2("'%s' should be composed only of 0s and 1s.", deparse(substitute(x)))
}

assert_multiple <- function(x) {

  nuniq <- length(unique(x))

  if (nuniq < 2) {
    stop2("'%s' should be composed of different values.", deparse(substitute(x)))
  } else if (nuniq == 2) {
    warning2("'%s' is composed of only two different levels.", deparse(substitute(x)))
  }
}

################################################################################

# CLASS
assert_class <- function(x, class)  {
  if (!inherits(x, class))
    stop2("'%s' is not of class '%s'.", deparse(substitute(x)), class)
}

################################################################################

# ALL SAME VALUE
assert_all <- function(x, value) {
  if (any(x != value))
    stop2("At least one value of '%s' is different from '%s'",
          deparse(substitute(x)), value)
}

################################################################################

# DIRECTORY
assert_dir <- function(dir.path) {
  if (!dir.exists(dir.path)) {
    if (dir.create(dir.path)) {
      message2("Creating directory \"%s\" which didn't exist..", dir.path)
    } else {
      stop2("Problem creating directory \"%s\". Recursive path?", dir.path)
    }
  }
}

################################################################################

# FILE EXISTS
assert_exist <- function(file) {
  if (!file.exists(file))
    stop2("File '%s' doesn't exist.", file)
}

assert_noexist <- function(file) {
  if (file.exists(file))
    stop2("File '%s' already exists.", file)
}

################################################################################

# ... not used
assert_nodots <- function() {

  list_dots <- eval(parse(text = "list(...)"), parent.frame())
  if (!identical(list_dots, list()))
    stop2("Argument '%s' not used.", names(list_dots[1]))
}

################################################################################
