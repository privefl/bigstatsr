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
#' @param nboot Number of bootstrap samples used to evaluate the 95\% CI.
#' Default is `1e4`.
#' @param seed See [set.seed]. Use it for reproducibility.
#' Default doesn't set any seed.
#' @param digits See [round]. Default doesn't use rounding.
#'
#' @seealso [wilcox.test]
#' @examples
#' set.seed(1)
#'
#' AUC(c(0, 0), 0:1) # Equality of scores
#' AUC(c(0.2, 0.1, 1), c(0, 0, 1)) # Perfect AUC
#' x <- rnorm(100)
#' z <- rnorm(length(x), x, abs(x))
#' y <- as.numeric(z > 0)
#' print(AUC(x, y))
#' print(AUCBoot(x, y))
#'
#' # Partial AUC
#' pAUC <- function(pred, target, p = 0.1) {
#'   val.min <- min(target)
#'   q <- quantile(pred[target == val.min], probs = 1 - p)
#'   ind <- (target != val.min) | (pred > q)
#'   bigstatsr::AUC(pred[ind], target[ind]) * p
#' }
#' pAUC(x, y)
#' pAUC(x, y, 0.2)
#' @name AUC
NULL

################################################################################

AUC2 <- function(pred, y) {
  ord <- order(pred, y)
  auc_sorted(pred[ord], y[ord])
}

round2 <- function(x, digits = NULL) `if`(is.null(digits), x, round(x, digits))

################################################################################

#' @rdname AUC
#' @name AUC
#' @export
AUC <- function(pred, target, digits = NULL) {

  assert_lengths(pred, target)
  assert_noNA(pred)
  assert_01(target)

  round2(AUC2(pred, as.logical(target)), digits)
}

################################################################################

#' @rdname AUC
#' @name AUCBoot
#' @export
AUCBoot <- function(pred, target, nboot = 1e4, seed = NA, digits = NULL) {

  assert_lengths(pred, target)
  assert_noNA(pred)
  assert_01(target)

  y <- as.logical(target)

  ord <- order(pred, y)
  pred <- pred[ord]
  y <- y[ord]

  if (!is.na(seed)) {
    # http://stackoverflow.com/a/14324316/6103040
    old <- .Random.seed
    on.exit( { .Random.seed <<- old } )
    set.seed(seed)
  }

  repl <- boot_auc_sorted_tab(pred, y, nboot)

  if (nbNA <- sum(is.na(repl)))
    warning2("%d/%d bootstrap replicates were mono-class.", nbNA, nboot)

  res <- c("Mean" = mean(repl, na.rm = TRUE),
           stats::quantile(repl, c(0.025, 0.975), na.rm = TRUE),
           "Sd" = stats::sd(repl, na.rm = TRUE))

  round2(res, digits)
}

################################################################################
