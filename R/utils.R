################################################################################

# Global variables
ALL.TYPES <- c("raw", "char", "short", "integer", "float", "double") # for tests
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

  if (Sys.info()["sysname"] == "Windows") {
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
