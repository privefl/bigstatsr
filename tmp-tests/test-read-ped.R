get_nlines <- function(file) {
  scan(text = system(paste("wc -l", file), intern = TRUE),
       what = 1L, n = 1, quiet = TRUE)
}

get_nelem <- function(file, split) {
  con <- file(file, open = "rt")
  l <- length(strsplit(readLines(con, n = 1), split = split, fixed = TRUE)[[1]])
  close(con)
  l
}

scan_chr <- function(con, n, sep) {
  scan(con, what = "", n = n, sep = sep, quiet = TRUE)
}

scan_dbl <- function(con, n, sep) {
  scan(con, n = n, sep = sep, quiet = TRUE)
}

scan_int <- function(con, n, sep) {
  scan(con, what = 1L, n = n, sep = sep, quiet = TRUE)
}

file <- "../POPRES_data/POPRES_allchr.ped"
split <- " "

con <- file(file, "rt")
close(con)
con.bin <- file(file, "rb")

readLines(file, n = 2)
scan(con, what = "", n = 5)
scan(con.bin, what = "", n = 5)
readChar(con, 10)

rawToChar(as.raw(4))
?charmatch
charToRaw("Croatia")


rawConnection(file)

str(readLines(file, n = 2))
m <- get_nlines(file)
n <- get_nelem(file, split)

con <- file(file, "rt")
ACTG <- c("A", "C", "T", "G")
scan_chr(con, 6, split)
test <- match(scan_chr(con, n - 6, split), ACTG)

fam <- matrix(NA_character_, 6, n)
library(bigmemory)
options(bigmemory.typecast.warning = FALSE)
res <- big.matrix(2*(n - 6), m, type = "char", shared = FALSE)

map <- data.table::fread("../POPRES_data/POPRES_allchr.map")

con <- file(file, "rt")
print(system.time(
  for (k in 1:m) {
    cat(k, "\n")
    fam[, k] <- scan_chr(con, 6, split)
    res[, k] <- match(scan_chr(con, n - 6, split), ACTG)
  }
)) # 4-5 min


test <- scan_chr(con, n = 1, sep = split)
stopifnot(test == character(0)) # EOF

close(con)

