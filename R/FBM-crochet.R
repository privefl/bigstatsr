################################################################################

# Transform negative or boolean indices to positive indices
transform_ind <- function(k, lim) {

  if (missing(k))
    return(seq_len(lim))

  if (is.character(k))
    stop2("Character subsetting is not allowed.")

  res <- seq_len(lim)[k]

  if (any(is.na(res)))
    stop2("Error when subsetting (missing values? out of bounds?")

  res
}

transform_i_only <- function(i, n, m) {

  if (is.logical(i))
    stop2("Logical vector subsetting is not allowed")

  if (!isTRUE(all(i > 0)))
    stop2("Only positive vector subsetting is allowed")

  if (is.matrix(i))
    i <- (transform_ind(i[, 2], m) - 1L) * n + transform_ind(i[, 1], n)

  if (any(i > (n * m)))
    stop2("Subscript out of bounds.")

  i
}

################################################################################

#' Create an Implementation of [ For Custom Matrix-Like Types
#'
#' `extract` is a function that converts different index types such as negative
#' integer vectors or logical vectors passed to the `[` function as `i`
#' (e.g. `X[i]`) or `i` and `j` (e.g. `X[i, j]`) into positive
#' integer vectors. The converted indices are provided as the `i` parameter of
#' `extract_vector` or `i` and `j` parameters of `extract_matrix` to facilitate
#' implementing the extraction mechanism for custom matrix-like types.
#'
#' The custom type must implement methods for [base::dim] for this function
#' to work. Implementing methods for [base::nrow] and [base::ncol] is not
#' necessary as the default method of those generics calls [base::dim]
#' internally.
#'
#' **This idea initially comes from [package crochet](https://goo.gl/3RDNQG).**
#'
#' @param extract_vector A function in the form of `function(x, i)` that takes
#' a subset of `x` based on a single vector of indices `i` and returns a vector.
#' @param extract_matrix A function in the form of `function(x, i, j)` that
#' takes a subset of `x` based on two vectors of indices `i` and `j` and returns
#' a matrix.
#'
#' @return A function in the form of `function(x, i, j, drop = TRUE)` that
#' is meant to be used as a method for \code{\link[base]{[}} for a custom type.
#'
extract <- function(extract_vector, extract_matrix) {

  function(x, i, j, drop = TRUE) {

    n <- nrow(x)
    m <- ncol(x)

    nargs <- nargs() - !missing(drop)

    if (nargs == 2) {

      if (missing(i)) {
        nargs <- 3  # x[] is the same as x[,]
      } else {
        return(extract_vector(x, transform_i_only(i, n, m)))
      }

    }

    if (nargs == 3) {

      res <- extract_matrix(x, transform_ind(i, n), transform_ind(j, m))

      return(`if`(drop, drop(res), res))

    }

  }

}


#' @export
`[.FBM` <- extract(
  extract_vector = function(x, i) extractVec(x$address, i),
  extract_matrix = function(x, i, j) extractMat(x$address, i, j)
)

################################################################################

#' Create an Implementation of [<- For Custom Matrix-Like Types
#'
#' `replace` is a function that converts different index types such as negative
#' integer vectors or logical vectors passed to the `[<-`
#' function as `i` (e.g. `X[i]`) or `i` and `j` (e.g. `X[i, j]`) into positive
#' integer vectors. The converted indices are provided as the `i` parameter of
#' `replace_vector` or `i` and `j` parameters of `replace_matrix` to facilitate
#' implementing the replacement mechanism for custom matrix-like types. Values
#' are recycled to match the replacement length.
#'
#' The custom type must implement methods for [base::dim] for this function
#' to work. Implementing methods for [base::nrow] and [base::ncol] is not
#' necessary as the default method of those generics calls [base::dim]
#' internally.
#'
#' **This idea initially comes from [package crochet](https://goo.gl/3RDNQG).**
#'
#' @param replace_vector A function in the form of `function(x, i, value)`
#' that replaces a vector subset of `x` based on a single vector of indices `i`
#' with the values in `value` and returns `x`, invisibly.
#' @param replace_matrix A function in the form of `function(x, i, j, value)`
#' that replaces a matrix subset of `x` based on two vectors of indices `i` and
#' `j` with the values in `value` and returns `x`, invisibly.
#'
#' @return A function in the form of `function(x, i, j, value)` that is
#' meant to be used as a method for \code{\link[base]{[<-}} for a custom type.
#'
replace <- function(replace_vector, replace_matrix) {

  function(x, i, j, value) {

    warn_downcast(from = value, to = x)

    n <- nrow(x)
    m <- ncol(x)

    nargs <- nargs()

    if (nargs == 3) {

      if (missing(i)) {
        nargs <- 4  # x[] is the same as x[,]
      } else {
        replace_vector(x, transform_i_only(i, n, m), value)
      }

    }

    if (nargs == 4) {

      replace_matrix(x, transform_ind(i, n), transform_ind(j, m), value)

    }

    invisible(x)

  }

}


#' @export
`[<-.FBM` <- replace(

  replace_vector = function(x, i, value) {
    if (length(value) == 1) {
      replaceVecOne(x$address, i, value)
    } else if (length(value) == length(i)) {
      replaceVec(x$address, i, value)
    } else {
      stop2("'value' must be unique or of the length of 'x[i]'.")
    }
  },

  replace_matrix = function(x, i, j, value) {
    if (length(value) == 1) {
      replaceMatOne(x$address, i, j, value)
    } else {
      .dim <- c(length(i), length(j))
      if (length(value) == prod(.dim)) {
        dim(value) <- .dim
        replaceMat(x$address, i, j, value)
      } else {
        stop2("'value' must be unique or of the dimension of 'x[i, j]'.")
      }
    }
  }

)

################################################################################
