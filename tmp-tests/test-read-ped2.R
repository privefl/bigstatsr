cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = "ex.data",
    sep = "\n")
readLines("ex.data", n = -1)
unlink("ex.data") # tidy up

tmp <- readLines(file, 2)
tmp[]

# true m
# effective m
# effective n
# transposed?
# type of reading
# ... same as transpose for BM + type
# number lines of header

pedfile <- system.file("extdata", "example.ped", package = "bigstatsr")
cat(readLines(pedfile), sep = "\n")
readLines(pedfile, 2)
ACTG <- c("A", "C", "T", "G")
ref <- match(c("T", "T", "G", "C", "C", "T", "G", "C"), ACTG)
split = " "

con <- file(pedfile, open = "rt")
scan_chr(con, 6, split)
read <- scan_chr(con, 22 - 6, split)

transfo <- function(read) {
  read.int <- match(read, ACTG)
  (read.int[c(TRUE, FALSE)] != ref) + (read.int[c(FALSE, TRUE)] != ref)
}

res.nline <- get_nlines(pedfile)
read.nelem <- get_nelem(pedfile, split) - 6
res.nelem <- read.nelem / 2
infos <- matrix(NA_character_, 6, res.nline)
res <- matrix(NA_integer_, res.nelem, res.nline)
nb.infos <- 6

con <- file(pedfile, open = "rt")
header <- readLines(pedfile, n = 0)
for (k in 1:res.nline) {
  if (nb.infos) infos[, k] <- scan_chr(con, nb.infos, split)
  res[, k] <- transfo(scan_chr(con, read.nelem, split))
}

res
header
infos
