################################################################################

context("TMP_FBM")

if (Sys.info()["sysname"] == "Windows") {
  message2("This test is skipped on Windows because of file permisions.")
  Sys.sleep(1)
} else {
  opt.save <- options(bigmemory.typecast.warning = FALSE,
                      bigmemory.default.shared = FALSE)

  l <- length(list.files(tempdir()))

  ##############################################################################

  test_that("Create temporary big.matrix objects", {
    for (t in ALL.TYPES) {
      tmp <- tmpFBM(init = sample(list(NULL, 1))[[1]],
                    descriptor = sample(c(TRUE, FALSE), 1))(10, 10, type = t)

      expect_equal(length(list.files(tempdir())), l + 2)

      expect_equal(tmpFBM.rm(tmp), c(TRUE, TRUE))
      expect_equal(length(list.files(tempdir())), l)
    }
  })

  ##############################################################################

  options(opt.save)

  ##############################################################################

}

