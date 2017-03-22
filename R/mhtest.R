################################################################################

#' Predict method
#'
#' Predict method for class `mhtest`.
#'
#' @param object An object of class `mhtest` from you get the probability
#' function with possibily pre-transformation of scores.
#' @param scores Raw scores (before transformation) that you want to transform
#' to p-values.
#' @param ... Not used.
#'
#' @return Vector of p-values associated with `scores` and `object`.
#' @export
#' @importFrom stats predict
#' @importFrom magrittr %>%
#'
#' @seealso [big_univLinReg] and [big_univLogReg].
predict.mhtest <- function(object, scores = object$score, ...)
  scores %>% attr(object, "transfo")() %>% attr(object, "predict")()

################################################################################

#' Plot method
#'
#' Plot method for class `mhtest`.
#'
#' @param x An object of class `mhtest`.
#' @param type Either
#' - "Manhattan": plot of the negative logarithm (in base 10) of p-values.
#' - "Volcaco": plot of the negative logarithm of p-values against the
#'   estimation of coefficients (e.g. betas in linear regression).
#' @param main The title of the plot. Default use the `type`.
#' @param cex Graphical parameter: relative size of points. Default is `0.5`.
#' @param pch Graphical parameter: type of points. Default uses "filled" points.
#' @param ... Other arguments to be passed to `plot()`.
#'
#' @return `NULL`. Creates a plot.
#' @export
#' @importFrom graphics plot
#'
#' @seealso [big_univLinReg] and [big_univLogReg].
plot.mhtest <- function(x, type = c("Manhattan", "Volcano"),
                        main = paste(type, "plot"),
                        cex = 0.5, pch = 19, ...) {

  lpval <- -log10(predict(x))
  YLAB <- expression(-log[10](italic("p-value")))

  type <- match.arg(type)
  if (type == "Manhattan") {
    plot(lpval, ylab = YLAB, main = main, cex = cex, pch = pch, ...)
  } else if (type == "Volcano") {
    plot(x[["estim"]], lpval, xlab = "estimate",
         ylab = YLAB, main = main, cex = cex, pch = pch, ...)
  }
}

################################################################################
