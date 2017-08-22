################################################################################

#' Recommended number of cores to use
#'
#' This is base on the following rule: use only physical cores and if you have
#' only physical cores, leave one core for the OS/UI.
#'
#' @inheritParams parallel::detectCores
#'
#' @return The recommended number of cores to use.
#' @export
#'
#' @seealso [parallel::detectCores]
#'
#' @examples
#' # Number of cores in total
#' parallel::detectCores()
#' # Number of physical cores
#' parallel::detectCores(logical = FALSE)
#' # Recommended number of cores to use
#' ncores()
ncores <- function(all.tests = FALSE) {
  all_cores <- parallel::detectCores(all.tests = all.tests)
  all_physical_cores <- parallel::detectCores(all.tests = all.tests,
                                              logical = FALSE)
  `if`(all_physical_cores < all_cores, all_physical_cores, all_cores - 1)
}

################################################################################
