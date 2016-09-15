colsums <- function(X, ind.train = seq_len(nrow(X))) {
  bigcolsums(X@address, ind.train)
}

colmeans <- function(X, ind.train = seq_len(nrow(X))) {
  colsums(X, ind.train) / length(ind.train)
}

colvars <- function(X, ind.train = seq_len(nrow(X))) {
  bigcolvars(X@address, ind.train)
}

colsds <- function(X, ind.train = seq_len(nrow(X))) {
  sqrt(colvars(X, ind.train))
}
