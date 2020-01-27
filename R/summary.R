################################################################################

#' Summary method
#'
#' Summary method for class `big_sp_list`.
#'
#' @param object An object of class `big_sp_list`.
#' @param best.only Whether to return only one row corresponding to the best
#'   model? The best model is the one smallest `$validation_loss`.
#' @param sort Whether to sort by `$validation_loss`. Default is `FALSE`.
#' @param ... Not used.
#'
#' @return A tibble with, for each `$alpha`, a mean `$validation_loss`, a mean
#'   vector of coefficients `$beta`, the corresponding number of non-zero
#'   coefficients `$nb_var`, and the reasons of method completion `$message`.
#'
#' @export
#' @import foreach
#' @importFrom tibble tibble
#'
summary.big_sp_list <- function(object, best.only = FALSE, sort = FALSE, ...) {

  assert_nodots()

  res <- foreach(mods = object, .combine = "rbind") %do% {
    tibble::tibble(
      alpha = mods[[1]]$alpha,
      validation_loss = mean(sapply(mods, function(mod) min(mod$loss.val))),
      intercept = mean(sapply(mods, function(mod) mod$intercept)),
      beta = list(rowMeans(sapply(mods, function(mod) mod$beta))),
      nb_var = sapply(beta, function(x) sum(x != 0)),
      message = list(sapply(mods, function(mod) mod$message)),
      all_conv = sapply(message, function(msg) all(msg == "No more improvement"))
    )
  }

  if (best.only) {
    res[which.min(res$validation_loss), ]
  } else if (sort) {
    res[order(res$validation_loss, -res$alpha), ]
  } else {
    res
  }
}

################################################################################
