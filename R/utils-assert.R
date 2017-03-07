################################################################################

# CLASS
assert_classOrDesc <- function(x, class)  {
  if (class(x) != class)
    if (class(x) != paste0(class, ".descriptor"))
      stop2("'%s' is not of class '%s' (or associated descriptor).",
            deparse(substitute(x)), class)
}

################################################################################

# ALL SAME VALUE
assert_all <- function(x, value) {
  if (any(x != value))
    stop2("At least one value of '%s' is different from '%s'",
          deparse(substitute(x)), value)
}

################################################################################
