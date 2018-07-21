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
if (Sys.info()[["sysname"]] == "Windows") {

  # https://sourceforge.net/projects/gnuwin32/
  awk <- shortPathName("C:/Program Files (x86)/GnuWin32/bin/awk.exe") # Windows
  cmd <- sprintf("%s \"NR%%%d==1{x=\"\"\"%s\"\"\"++i;}{print > x}\" %s",
                 awk, 20, gsub("\\\\", "\\\\\\\\", tmp), normalizePath(csv))

} else {

  cmd <- sprintf("awk 'NR%%%d==1{x=\"%s\"++i;}{print > x}' %s",
                 tmp, 20, normalizePath(csv))

}
system(cmd)
readLines(paste0(tmp, 1), 1)

cmd <- sprintf("%s \"NR%%%d==1{x=\"\"\"%s\"\"\"++i;}{print > x}\" %s",
               awk, 20000, gsub("\\\\", "\\\\\\\\", tmp), normalizePath(csv2))
system.time(system(cmd)) # 1.4
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

cmd <- sprintf("%s \"NR%%%d==1{x=\"\"\"%s\"\"\"++i;}{print > x}\" %s",
               awk, 2, gsub("\\\\", "\\\\\\\\", tmp), normalizePath(csv3))
system.time(system(cmd)) # 1.4
# readLines(paste0(tmp, 1))

