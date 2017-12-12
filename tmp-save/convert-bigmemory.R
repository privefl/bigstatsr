library(bigmemory)

tmp <- tempfile()
bm <- big.matrix(10, 10, backingfile = paste0(basename(tmp), ".bk"),
                 descriptorfile = paste0(basename(tmp), ".desc"),
                 backingpath = dirname(tmp))
bm[]

dir.name(bm)

desc <- describe(bm)

library(bigstatsr)
fbm <- FBM(nrow = nrow(bm), ncol = ncol(bm), type = typeof(bm),
           backingfile = file.path(dir.name(bm), sub("\\.bk$", "", file.name(bm))),
           create_bk = FALSE)

fbm[]
bm[1, 1] <- 1
fbm[]

desc
new_desc <- new("big.matrix.descriptor",
                description = list(
                  sharedType = "FileBacked",
                  filename   = basename(fbm$backingfile),
                  dirname    = bigmemory:::format_path(new_desc@description$dirname),
                  totalRows  = nrow(fbm),
                  totalCols  = ncol(fbm),
                  rowOffset  = c(0, nrow(fbm)),
                  colOffset  = c(0, ncol(fbm)),
                  nrow       = as.double(nrow(fbm)),
                  ncol       = as.double(ncol(fbm)),
                  rowNames   = NULL,
                  colNames   = NULL,
                  type       = typeof(fbm),
                  separated  = FALSE
                ))
new_desc
new_bm <- attach.big.matrix(new_desc)
new_bm[]
fbm[] <- 2
new_bm[]
