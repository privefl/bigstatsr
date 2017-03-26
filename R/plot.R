################################################################################

MY_THEME <- function(p, coeff = 1) {
  p + theme_bw() +
    theme(plot.title    = element_text(size = rel(2.0 * coeff), hjust = 0.5),
          plot.subtitle = element_text(size = rel(1.5 * coeff), hjust = 0.5),
          legend.title  = element_text(size = rel(1.5 * coeff)),
          legend.text   = element_text(size = rel(1.2 * coeff)),
          axis.title    = element_text(size = rel(1.5 * coeff)),
          axis.text     = element_text(size = rel(1.2 * coeff)))
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
#' @param loadings Indices of PC loadings to plot. Default plots the
#' first vector of loadings.
#' @param cols If multiple vector of loadings are to be plotted, this defines
#' the number of columns of the resulting multiplot.
#' @param coeff Relative size of text. Default is `1`.
#' @param ... Not used.
#'
#' @return A `ggplot2` object. You can plot it using the `print` method.
#' You can modify it as you wish by adding layers. You might want to read
#' [this chapter](http://r4ds.had.co.nz/data-visualisation.html)
#' to get more familiar with the package **ggplot2**.
#' @export
#' @import ggplot2 grid
#' @importFrom graphics plot
#'
#' @example examples/example-plot-bigSVD.R
#' @seealso [big_SVD], [big_randomSVD] and [asPlotlyText].
plot.big_SVD <- function(x, type = c("screeplot", "scores", "loadings"),
                         nval = length(x$d),
                         scores = c(1, 2),
                         loadings = 1,
                         cols = 2,
                         coeff = 1,
                         ...) {

  stopifnot(length(nval) == 1)
  stopifnot(length(scores) == 2)

  type <- match.arg(type)

  if (type == "screeplot") {

    p <- MY_THEME(qplot(y = x$d[seq_len(nval)]), coeff = coeff) +
      geom_line() +
      labs(title = "Scree Plot", x = "PC Index", y = "Singular Values")

    `if`(nval > 12, p, p + scale_x_discrete(limits = seq_len(nval)))

  } else if (type == "scores") {

    sc <- predict(x)
    nx <- scores[1]
    ny <- scores[2]

    MY_THEME(qplot(x = sc[, nx], y = sc[, ny]), coeff = coeff) +
      labs(title = "Scores of PCA", x = paste0("PC", nx), y = paste0("PC", ny))

  } else if (type == "loadings") {

    if (length(loadings) > 1) {

      all.p <- lapply(1:10, function(i) {
        p <- plot(x, type = "loadings", loading = i, coeff = coeff)
        p$layers[[1]] <- NULL
        p + geom_hex() + viridis::scale_fill_viridis()
      })

      multiplot(plotlist = all.p, cols = cols)

    } else {

      p <- MY_THEME(qplot(y = x$v[, loadings]), coeff = coeff) +
        labs(title = paste0("Loadings of PC", loadings),
             x = "Column index", y = NULL)

      nval <- nrow(x$v)
      `if`(nval > 12, p, p + scale_x_discrete(limits = seq_len(nval)))

    }

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
#' - "Q-Q": Q-Q plot.
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
#' set.seed(1)
#'
#' X.desc <- big_attachExtdata()
#' y <- rnorm(nrow(X.desc))
#' test <- big_univLinReg(X.desc, y)
#'
#' plot(test)
#' plot(test, type = "Volcano")
#' plot(test, type = "Q-Q")
#'
#' @seealso [big_univLinReg], [big_univLogReg],
#' [plot.big_SVD] and [asPlotlyText].
plot.mhtest <- function(x, type = c("Manhattan", "Q-Q", "Volcano"),
                        main = paste(type, "Plot"),
                        coeff = 1,
                        ...) {

  lpval <- predict(x) # log10(p)

  type <- match.arg(type)

  if (type == "Manhattan") {
    MY_THEME(qplot(y = -lpval), coeff = coeff) +
      labs(title = main, x = "Column Index",
           y = expression(-log[10](italic("p-value"))))
  } else if (type == "Volcano") {
    MY_THEME(qplot(x = x[["estim"]], y = -lpval), coeff = coeff) +
      labs(title = main, x = "Estimate",
           y = expression(-log[10](italic("p-value"))))
  } else if (type == "Q-Q") {
    unif.ranked <- ppoints(length(lpval))[rank(lpval)]
    MY_THEME(qplot(x = -log10(unif.ranked), y = -lpval),
             coeff = coeff) +
      labs(title = main,
           x = expression(Expected~~-log[10](italic("p-value"))),
           y = expression(Observed~~-log[10](italic("p-value")))) +
      geom_abline(slope = 1, intercept = 0, color = "red")
  }
}

################################################################################

#' Plotly text
#'
#' Convert a data.frame to plotly text
#'
#' @param df A data.frame
#'
#' @return A character vector of the length of `df`'s number of rows.
#' @export
#' @import foreach
#'
#' @examples
#' set.seed(1)
#'
#' test <- big_attachExtdata()
#' svd <- big_SVD(test, big_scale(), k = 10)
#'
#' p <- plot(svd, type = "scores")
#'
#' pop <- rep(c("POP1", "POP2", "POP3"), c(143, 167, 207))
#' df <- data.frame(Population = pop, Index = 1:517)
#'
#' plot(p2 <- p + ggplot2::aes(text = asPlotlyText(df)))
#' \dontrun{plotly::ggplotly(p2, tooltip = "text")}
asPlotlyText <- function(df) {
  paste.br <- function(lhs, rhs) paste(lhs, rhs, sep = "<br>")
  foreach(ic = seq_along(df), .combine = "paste.br") %do% {
    paste(names(df)[ic], df[[ic]], sep = ": ")
  }
}

################################################################################
