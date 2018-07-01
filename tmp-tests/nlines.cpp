// COPIED FROM https://github.com/privefl/fpeek/blob/master/src/fpeek.cpp

#include <Rcpp.h>
#include <fstream>

// [[Rcpp::export]]
int nlines_(std::string filename) {
  std::ifstream is(filename.c_str(), std::ifstream::binary);
  int count = 0;
  if( !is )
    Rcpp::stop("error while opening filename");

  std::istream::sentry se(is, true);
  std::streambuf* sb = is.rdbuf();

  for(;;) {
    int c = sb->sbumpc();
    if( c == '\n'){
      count++;
    } else if( c == '\r'){
      if(sb->sgetc() == '\n'){
        sb->sbumpc();
      }
      count++;
    } else if( c == std::streambuf::traits_type::eof() ){
      // count++;
      break;
    }
  }

  is.close();
  return count;
}

/***R
# power_file <- "tmp-data/singleindex.csv"
# if( !file.exists(power_file)) {
#   download.file(
#     "https://data.open-power-system-data.org/time_series/2018-03-13/time_series_15min_singleindex.csv",
#     destfile = power_file)
# }
# system.time(print(nlines_(power_file)))
# # system.time(print(R.utils::countLines(power_file)))
# readLines(power_file, n = 1)
#
#
# # Define only the subset of columns
# csv <- power_file
# colnames <- names(read.csv(csv, header = TRUE, nrows = 1))
# colclasses <- rep(list(NULL), length(colnames))
# ind <- c(1, 2, 7, 12, 15)
# colclasses[ind] <- "double"
#
# # Read header and first line
# library(dplyr)
# l_df <- list()
# con <- file(csv, "rt")
# df <- read.csv(con, header = TRUE, nrows = 1, colClasses = colclasses) %>%
#   filter(V1 == 6, V7 == 1)
# names(df) <- paste0("V", ind)
# l_df[[i <- 1]] <- df
#
# # Read all other lines and combine
# repeat {
#   i <- i + 1
#   df <- read.csv(con, header = FALSE, nrows = 9973, colClasses = colclasses)
#   l_df[[i]] <- filter(df, V1 == 6, V7 == 1)
#   if (nrow(df) < 9973) break
# }
# df <- do.call("rbind", l_df)
*/
