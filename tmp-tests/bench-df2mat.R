mat <- matrix(0, 150, 4)
mat[] <- iris[1:4]
mat2 <- matrix(0, 150, 4)
mat2[,] <- iris[1:4]
mat3 <- matrix(0, 150, 4)
mat3[, 1:4] <- iris[1:4]

as_matrix1 <- function(df) {
  as.matrix(df)
}

as_matrix2 <- function(df) {
  do.call(cbind, df)
}

as_matrix3 <- function(df) {
  vec <- unlist(df)
  dim(vec) <- dim(df)
  vec
}

microbenchmark::microbenchmark(
  as_matrix1(iris),
  as_matrix2(iris),
  as_matrix3(iris),
  as_matrix1(iris[1:4]),
  as_matrix2(iris[1:4]),
  as_matrix3(iris[1:4]),
  as_matrix1(mtcars),
  as_matrix2(mtcars),
  as_matrix3(mtcars)
)
