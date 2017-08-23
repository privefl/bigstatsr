################################################################################

# Global variables
ALL.TYPES <- structure(c(1L, 1L, 2L, 4L, 8L),
                       names = c("raw", "unsigned char", "unsigned short",
                                 "integer", "double"))
globalVariables("ic") # for foreach

################################################################################

# function for comparing PCs
diffPCs <- function(test, rot) {
  k <- ncol(test)
  diff1 <- 2 * abs(test - rot[, 1:k]) / (abs(test) + abs(rot[, 1:k]))
  diff2 <- 2 * abs(test + rot[, 1:k]) / (abs(test) + abs(rot[, 1:k]))
  diff <- pmin(diff1, diff2)
  mean(diff)
}

################################################################################

printf <- function(...) cat(sprintf(...))
message2 <- function(...) message(sprintf(...))
warning2 <- function(...) warning(sprintf(...), call. = FALSE)
stop2 <- function(...) stop(sprintf(...), call. = FALSE)

################################################################################

#' Determine a correct value for the block.size parameter
#'
#' It determines the value of `block.size` such that a matrix of doubles of
#' size `n` x `block.size` takes less memory than
#' `getOption("bigstatsr.block.sizeGB")` GigaBytes (default is 1GB).
#'
#' @param n The number of rows.
#' @param ncores The number of cores.
#'
#' @return An integer >= 1.
#'
#' @export
#'
#' @examples
#' block_size(1e3)
#' block_size(1e6)
#' block_size(1e6, 6)
block_size <- function(n, ncores = 1) {
  block.max <- getOption("bigstatsr.block.sizeGB") / ncores
  # 8 * n * m < opt * 1024^3
  # m < opt * 1024^3 / (8 * n)
  max(1, floor(block.max * 1024^3 / (8 * n)))
}

################################################################################

# TODO: case when nb > m
CutBySize <- function(m, block.size, nb = ceiling(m / block.size)) {
  int <- m / nb

  upper <- round(1:nb * int)
  lower <- c(1, upper[-nb] + 1)
  size <- c(upper[1], diff(upper))

  cbind(lower, upper, size)
}

################################################################################

seq2 <- function(lims) {
  seq(lims[1], lims[2])
}

################################################################################

transform_levels <- function(y, new.levels = 0:1) {
  y2 <- factor(y, ordered = TRUE)
  lvl <- levels(y2)
  if (length(lvl) != 2)
    stop("You must have exactly two levels in y.")
  levels(y2) <- new.levels
  as.numeric(as.character(y2))
}

################################################################################

getAvailMem <- function(format = TRUE) {

  gc()

  if (Sys.info()[["sysname"]] == "Windows") {
    memfree <- 1024^2 * (utils::memory.limit() - utils::memory.size())
  } else {
    # http://stackoverflow.com/a/6457769/6103040
    memfree <- 1024 * as.numeric(
      system("awk '/MemFree/ {print $2}' /proc/meminfo", intern = TRUE))
  }

  `if`(format, format(structure(memfree, class = "object_size"),
                      units = "auto"), memfree)
}

################################################################################
#### Sequence generation ####

#' Sequence generation
#'
#' Similar to [seq_along], it creates sequences of size `nrow(x)` or `ncol(x)`.
#'
#' @param x Any object on which you can call `nrow` and `ncol`.
#'
#' @examples
#' X <- big_attachExtdata()
#' dim(X)
#' str(rows_along(X))
#' str(cols_along(X))
#'
#' @rdname seq-dim
#' @keywords internal
#' @export
rows_along <- function(x) seq_len(nrow(x))

#' @rdname seq-dim
#' @export
cols_along <- function(x) seq_len(ncol(x))

################################################################################
