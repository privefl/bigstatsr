pedfile <- system.file("extdata", "example.ped", package = "bigstatsr")
cat(readLines(pedfile), sep = "\n")
readLines(pedfile, 2)
ACTG <- c("A", "C", "T", "G")
ref <- match(c("T", "T", "G", "C", "C", "T", "G", "C"), ACTG)
split = " "

transfo <- function(read) {
  read.int <- match(read, ACTG)
  (read.int[c(TRUE, FALSE)] != ref) + (read.int[c(FALSE, TRUE)] != ref)
}

test <- big_readBM(file = pedfile,
                   file.nheader = 0,
                   info.nelem = 6,
                   split = " ",
                   read.what = character(),
                   read.transfo = transfo,
                   descriptor = FALSE)
test[,]
attributes(test)

# second file

file <- "../thesis-celiac/papillon/project_data"
readLines(file, n = 2)
test2 <- big_readBM(file,
                    file.nheader = 1,
                    info.nelem = 2,
                    split = "\t",
                    read.what = integer(),
                    descriptor = FALSE)
test2[, 1:10]
info <- attr(test2, "info")
info[, 1:10]
header <- attr(test2, "header")
header <- strsplit(header, split = "\t", fixed = TRUE)[[1]]
