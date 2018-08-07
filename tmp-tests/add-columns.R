N <- M <- 5000
system.time({
  X <- FBM(N, M)
})
file.size(X$backingfile)

system.time({
  # file.create(tmp <- tempfile())
  X2 <- FBM(N, 1)
  addColumns(X2$backingfile, N, M - 1, 8)
  X2 <- FBM(N, M, backingfile = sub("\\.bk$", "", X2$backingfile),
            create_bk = FALSE)
})
file.size(X2$backingfile)
