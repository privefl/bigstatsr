################################################################################

warn_downcast <- function(from, to) {

  if (getOption("bigstatsr.typecast.warning")) {

    from.type <- typeof(from)
    to.type   <- typeof(to)

    from.type.int <- try(ALL.TYPES[[from.type]], silent = TRUE)
    if (class(from.type.int) == "try-error") {
      warning(glue::glue(
        "The type of the input is unknown.\n",
        "Assignment could possibly down cast from {from.type} to {to.type}.\n",
        "Hint: To remove this warning, use ",
        "options(bigstatsr.typecast.warning = FALSE)."), call. = FALSE)
    } else {
      if (from.type.int > ALL.TYPES[[to.type]])
        warning(glue::glue(
          "Assignment will down cast from {from.type} to {to.type}.\n",
          "Hint: To remove this warning, use ",
          "options(bigstatsr.typecast.warning = FALSE)."), call. = FALSE)
    }
  }
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
        fun.createBM = "assert_args(fun.createBM, c('nrow', 'ncol', 'type'))",
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
  if (sum(is.na(attach.BM(x)[, sample(ncol(x), min(10, ncol(x)))])))
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
                 " We advise you to use `ncores()`.\n",
                 "You can change this default value with",
                 " `options(bigstatsr.ncores.max = Inf)`."))
  }
}

################################################################################

assert_lengths <- function(...) {
  lengths <- lengths(list(...))
  if (length(lengths) > 1) {
    if (any(diff(lengths) != 0))
      stop2("Incompatibility between dimensions.")
  } else {
    stop2("You should check the lengths of at least two elements.")
  }
}

################################################################################

# INTEGERS
assert_int <- function(x) {
  if (any(x != as.integer(x)))
    stop2("'%s' should contain only integers.", deparse(substitute(x)))
}

################################################################################

# POSITIVE INDICES
assert_pos <- function(x)  {
  if (!all(x > 0))
    stop2("'%s' should have only positive values.", deparse(substitute(x)))
}

################################################################################

# 0s AND 1s
assert_01 <- function(x, type)  {
  if (!all(x %in% 0:1))
    stop2("'%s' should be composed only of 0s and 1s.", deparse(substitute(x)))
}

################################################################################

# TYPEOF
assert_type <- function(x, type)  {
  if (typeof(x) != type)
    stop2("'%s' is not of type '%s'.", deparse(substitute(x)), type)
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
