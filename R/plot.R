################################################################################

MY_THEME <- function(p, title = NULL, coeff = 1) {
  p + theme_bw() + ggtitle(title) +
    theme(plot.title   = element_text(size = rel(2.0 * coeff), hjust = 0.5),
          legend.title = element_text(size = rel(1.5 * coeff)),
          legend.text  = element_text(size = rel(1.2 * coeff)),
          axis.title   = element_text(size = rel(1.5 * coeff)),
          axis.text    = element_text(size = rel(1.2 * coeff)))
}

################################################################################

#' Plot method
#'
#' Plot method for class `big_SVD`.
#'
#' @param x An object of class `big_SVD`.
#' @param type Either
#' - "screeplot": plot of decresing singular values (the default).
#' - "scores": plot of the scores associated with 2 Principal Components.
#' - "loadings": plot of loadings associated with 1 Principal Component.
#' @param nval Number of singular values to plot. Default plots all computed.
#' @param scores Vector of indices of the two PCs to plot. Default plots the
#' first 2 PCs.
#' @param loading Index of the unique PCA loading to plot. Default plots the
#' first vector of loadings.
#' @param coeff Relative size of text. Default is `1`.
#' @param ... Not used.
#'
#' @return A `ggplot2` object. You can plot it using the `print` method.
#' You can modify it as you wish by adding layers. You might want to read
#' [this chapter](http://r4ds.had.co.nz/data-visualisation.html)
#' to get more familiar with the package **ggplot2**. \cr
#' Do you want dynamic plots? Go check \url{https://plot.ly/ggplot2}.
#' @export
#' @import ggplot2
#' @importFrom graphics plot
#'
#' @example examples/example-plot-bigSVD.R
#' @seealso [big_SVD] and [big_randomSVD].
plot.big_SVD <- function(x, type = c("screeplot", "scores", "loadings"),
                         nval = length(x$d),
                         scores = c(1, 2),
                         loading = 1,
                         coeff = 1,
                         ...) {

  stopifnot(length(nval) == 1)
  stopifnot(length(scores) == 2)
  stopifnot(length(loading) == 1)

  type <- match.arg(type)
  if (type == "screeplot") {
    p <- MY_THEME(qplot(y = x$d[seq_len(nval)]),
             title = "Screeplot",
             coeff = coeff) +
      geom_line() +
      xlab("PC index") +
      ylab("Singular Values")
    `if`(nval > 12, p, p + scale_x_discrete(limits = seq_len(nval)))
  } else if (type == "scores") {
    sc <- predict(x)
    nx <- scores[1]
    ny <- scores[2]
    MY_THEME(qplot(x = sc[, nx], y = sc[, ny]),
             title = "Scores of PCA",
             coeff = coeff) +
      xlab(paste0("PC", nx)) +
      ylab(paste0("PC", ny))
  } else if (type == "loadings") {
    p <- MY_THEME(qplot(y = x$v[, loading]),
             title = paste0("Loadings of PC", loading),
             coeff = coeff) +
      xlab("Column index") +
      ylab(NULL)
    nval <- nrow(x$v)
    `if`(nval > 12, p, p + scale_x_discrete(limits = seq_len(nval)))
  }
}

################################################################################

#' Plot method
#'
#' Plot method for class `mhtest`.
#'
#' @param x An object of class `mhtest`.
#' @param type Either
#' - "Manhattan": plot of the negative logarithm (in base 10) of p-values
#'   (the default).
#' - "Volcaco": plot of the negative logarithm of p-values against the
#'   estimation of coefficients (e.g. betas in linear regression).
#' @param main The title of the plot. Default use the `type`.
#' @param coeff Relative size of text. Default is `1`.
#' @param ... Not used.
#'
#' @inherit plot.big_SVD return
#' @export
#' @import ggplot2
#' @importFrom graphics plot
#'
#' @examples
#' X.desc <- big_attachExtdata()
#' n <- nrow(X.desc)
#' y <- rnorm(n)
#' test <- big_univLinReg(X.desc, y)
#'
#' plot(test)
#' plot(test, type = "Volcano")
#'
#' @seealso [big_univLinReg], [big_univLogReg] and [plot.big_SVD].
plot.mhtest <- function(x, type = c("Manhattan", "Volcano"),
                        main = paste(type, "plot"),
                        coeff = 1,
                        ...) {

  lpval <- -log10(predict(x))
  YLAB <- expression(-log[10](italic("p-value")))

  type <- match.arg(type)
  if (type == "Manhattan") {
    MY_THEME(qplot(y = lpval), title = main, coeff = coeff) +
      xlab("Column index") +
      ylab(YLAB)
  } else if (type == "Volcano") {
    MY_THEME(qplot(x = x[["estim"]], y = lpval), title = main, coeff = coeff) +
      xlab("Estimate") +
      ylab(YLAB)
  }
}

################################################################################
