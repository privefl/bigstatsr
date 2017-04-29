################################################################################

#' AUC
#'
#' Compute the Area Under the ROC Curve (AUC) of a predictor
#' and possibly its 95\% confidence interval.
#'
#' @details Other packages provide ways to compute the AUC (see this
#' [answer](http://stats.stackexchange.com/a/146174/135793)).
#' I chose to compute the AUC through its statistical definition as a
#' probability: \deqn{P(score(x_{case}) > score(x_{control})).}
#' Note that I consider equality between scores as a 50\%-probability of
#' one being greater than the other.
#'
#' @return The AUC, a probability, and possibly its 2.5\% and 97.5\% quantiles
#' (95\% CI).
#'
#' @param pred Vector of predictions.
#' @param target Vector of true labels (must have exactly two levels,
#' no missing values).
#' @param nboot Number of bootstrap samples to evaluate the 95\% CI.
#' Default is `1e3`.
#' @param seed See [set.seed]. Use it for reproducibility.
#' Default doesn't set any seed.
#' @param digits See [round]. Default doesn't use rounding.
#'
#' @references
#' Hanley, J. A., & McNeil, B. J. (1982).
#' The meaning and use of the area under a
#' receiver operating characteristic (ROC) curve.
#' Radiology, 143(1), 29-36.
#' \url{http://dx.doi.org/10.1148/radiology.143.1.7063747}.
#'
#' Tom Fawcett. 2006. An introduction to ROC analysis.
#' Pattern Recogn. Lett. 27, 8 (June 2006), 861-874.
#' \url{http://dx.doi.org/10.1016/j.patrec.2005.10.010}.
#'
#' @seealso [wilcox.test]
#' @examples
#' set.seed(1)
#'
#' AUC(c(0, 0), 0:1) # Equality of scores
#' AUC(c(0.2, 0.1, 1), c(-1, -1, 1)) # Perfect AUC
#' x <- rnorm(100)
#' z <- rnorm(length(x), x, abs(x))
#' y <- sign(z)
#' print(AUC(x, y))
#' print(AUCBoot(x, y))
#' @name AUC
NULL

################################################################################

AUC2 <- function(pred, y) auc_cpp(pred[y == 1], pred[y == 0])

round2 <- function(x, digits = NULL) `if`(is.null(digits), x, round(x, digits))

################################################################################

#' @rdname AUC
#' @name AUC
#' @export
AUC <- function(pred, target, digits = NULL) {

  assert_lengths(pred, target)

  y <- transform_levels(target)

  round2(AUC2(pred, y), digits)
}

################################################################################

#' @rdname AUC
#' @name AUCBoot
#' @export
AUCBoot <- function(pred, target, nboot = 1e3, seed = NA, digits = NULL) {

  assert_lengths(pred, target)

  y <- transform_levels(target)
  n <- length(y)

  if (!is.na(seed)) {
    # http://stackoverflow.com/a/14324316/6103040
    old <- .Random.seed
    on.exit( { .Random.seed <<- old } )
    set.seed(seed)
  }

  repl <- replicate(nboot, {
    ind <- sample(n, replace = TRUE)
    AUC2(pred[ind], y[ind])
  })

  if (nbNA <- sum(is.na(repl)))
    warning2("%d/%d bootstrap replicates were mono-class.", nbNA, nboot)

  res <- c("Mean" = mean(repl, na.rm = TRUE),
           stats::quantile(repl, c(0.025, 0.975), na.rm = TRUE),
           "Sd" = stats::sd(repl, na.rm = TRUE))

  round2(res, digits)
}

################################################################################
