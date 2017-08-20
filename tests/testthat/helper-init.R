################################################################################

library(Matrix)

################################################################################

TEST.TYPES <- c("raw", "unsigned short", "integer", "double")

asFBMcode <- function(x) {
  x <- round(x)
  tmp <- unique(as.vector(x))
  code <- rep(NA_real_, 256)
  code[tmp + 1] <- tmp
  storage.mode(x) <- "raw"
  add_code256(big_copy(x, type = "raw"), code)
}

################################################################################

opt.save <- options(bigstatsr.typecast.warning = FALSE)

################################################################################
