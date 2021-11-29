################################################################################

is_singular <- function(m) {
  !is.matrix(try(solve(crossprod(m)), silent = TRUE))
}

################################################################################

#' Partial correlation
#'
#' Partial correlation between x and y, after having adjusted both for z.
#'
#' @return The partial correlation, and the lower and upper bounds of its CI.
#'
#' @param x A numeric vector.
#' @param y A numeric vector.
#' @param z A data frame, which can contain characters or factors.
#' @param alpha Type-I error for the confidence interval (CI).
#'   Default is `0.05`, corresponding to a 95% CI.
#'
#' @export
#'
#' @examples
#' pcor(iris[[1]], iris[[2]], iris[-(1:2)])
#'
pcor <- function(x, y, z, alpha = 0.05) {

  if (is.null(z)) z <- matrix(0, length(x), 0)

  assert_lengths(x, y, rows_along(z))

  m <- stats::model.matrix(~ ., data = cbind.data.frame(x, y, z))
  if (nrow(m) < 2) return(rep(NA_real_, 3))

  to_keep <- apply(m, 2, function(x) any(x != x[1]))
  to_keep[1:3] <- TRUE
  if (any(!to_keep)) {
    warning2("Discarding some covariates in `z` with no variation..")
    m <- m[, to_keep]
  }

  if (is_singular(m[, -2]) | is_singular(m[, -3]))
    return(rep(NA_real_, 3))

  mod1 <- stats::lm.fit(x = m[, -(2:3), drop = FALSE], y = m[, 2])
  mod2 <- stats::lm.fit(x = m[, -(2:3), drop = FALSE], y = m[, 3])

  if (mod1$df.residual < 3 || mod2$df.residual < 3) {
    rep(NA_real_, 3)
  } else {
    r <- stats::cor(mod1$residuals, mod2$residuals)
    # Fisher's Z-transformation
    z <- (log(1 + r) - log(1 - r)) / 2
    rad <- stats::qnorm(alpha / 2, lower.tail = FALSE) / sqrt(mod2$df.residual - 2)
    c(r, tanh(z - rad), tanh(z + rad))
  }
}

################################################################################
