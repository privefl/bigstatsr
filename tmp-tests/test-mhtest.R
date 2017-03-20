X.desc <- big_attachExtdata()
n <- nrow(X.desc)
y <- rnorm(n)
covar <- matrix(rnorm(n * 3), n)

X1 <- attach.BM(X.desc)[, 1] # only first column of the `big.matrix`

# Without covar
str(tmp <- big_univLinReg(X.desc, y))
summary(lm(y ~ X1))$coefficients[2, ]

tmp2 <- tmp[1, ]
str(tmp2)

predict.mhtest <- function(x, scores) {

  # if needed, transformation of scores (typically abs or square)
  if (!is.null(f <- attr(x, "transfo"))) scores <- f(scores)

  attr(x, "predict")(scores)
}

plot.mhtest <- function(x, type = c("Manhattan", "Volcano"),
                        main = paste(type, "plot"),
                        cex = 0.5, pch = 19, ...) {

  lpval <- -log10(x[["p.value"]])
  YLAB <- expression(-log[10](italic("p-value")))

  type <- match.arg(type)
  if (type == "Manhattan") {
    plot(lpval, ylab = YLAB, main = main, cex = cex, pch = pch, ...)
  } else if (type == "Volcano") {
    plot(x[["estim"]], lpval, xlab = "estimate",
         ylab = YLAB, main = main, cex = cex, pch = pch, ...)
  }
}
plot(tmp)
plot(tmp, type = "Volcano")
plot(tmp, main = NULL)


f1 <- function(x) (attr(tmp, "predict")(x) - 0.5)^2
optimize()
