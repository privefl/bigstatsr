################################################################################

context("ATTACH")

################################################################################

desc <- big_attachExtdata()
expect_s4_class(desc, "big.matrix.descriptor")
X <- attach.BM(desc)
expect_s4_class(X, "big.matrix")

################################################################################

tmpfile <- tempfile(fileext = ".pcadapt")
write.table(t([]), tmpfile, quote = FALSE, sep = " ",
            row.names = FALSE, col.names = FALSE)

x <- pcadapt::pcadapt(tmpfile, K = 10, min.maf = 0)

################################################################################

linRegPcadapt(X)

################################################################################
