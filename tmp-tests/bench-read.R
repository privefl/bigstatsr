library(readr)

csv <- readr_example("mtcars.csv")
df <- data.table::fread(csv, data.table = FALSE)

## LONG CSV
csv2 <- tempfile(fileext = ".csv")
data.table::fwrite(df[rep(seq_len(nrow(df)), 100000), ], csv2,
                   quote = FALSE, row.names = FALSE)

system.time(
  df2 <- data.table::fread(csv2)
) # 0.6

system.time(
  df3 <- readr::read_csv(csv2)
) # 4.9

tmp <- tempfile()
system.time({
  system(sprintf("awk 'NR%%20000==1{x=\"%s\"++i;}{print > x}' %s",
                 tmp, normalizePath(csv2)))
}) # 1.4
# readLines(paste0(tmp, 1))


## LARGE CSV
csv3 <- tempfile(fileext = ".csv")
data.table::fwrite(df[rep(seq_len(ncol(df)), 1000)], csv3,
                   quote = FALSE, row.names = FALSE)

system.time(
  df2 <- data.table::fread(csv3, data.table = FALSE)
) # 0.06

system.time(
  df3 <- readr::read_csv(csv3)
) # 6

tmp <- tempfile()
system.time({
  system(sprintf("awk 'NR%%2==1{x=\"%s\"++i;}{print > x}' %s",
                 tmp, normalizePath(csv3)))
}) # 1.4
readLines(paste0(tmp, 1))

