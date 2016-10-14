\dontrun{

  X <- big.matrix(4, 4)
  check_X(X) # OK
  mat <- as.matrix(X)
  check_X(mat) # NOK

  y <- rnorm(4)
  check_X(X, y, y.type = "reg") # OK
  check_X(X, y, y.type = "class") # NOK
}
