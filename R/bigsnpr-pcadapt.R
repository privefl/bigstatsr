#' T-scores used in pcadapt
#'
#' Compute matrix of t-scores (SNPs x scores) used in pcadapt.
#'
#' This function is used by package bigsnpr.
#'
#' @param BM A `BM.code` object.
#' @param U Matrix of left singular vectors (from partial SVD).
#' @param rowInd Vector of row indices of the `big.matrix` that are used.
#' @param colInd Vector of column indices of the `big.matrix` that are used.
#'
#' @return A matrix of t-scores where rows correspond to each SNP and
#' columns correspond to each left singular vector.
#'
#' @references Keurcien Luu and Michael Blum (2017).
#' pcadapt: Fast Principal Component Analysis for Outlier Detection.
#' R package version 3.0.4. https://CRAN.R-project.org/package=pcadapt.
#'
#' @export
#' @keywords internal
linRegPcadapt <- function(BM, U, rowInd, colInd)
  linRegPcadapt_cpp(BM, U, rowInd, colInd)
