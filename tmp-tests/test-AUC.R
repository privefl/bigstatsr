# x <- c(1.5:6, 1.5, 1.5, 2.5, 3.5, 5.5)
x <- rep(1, 10)
y <- rep(1:0, each = 5)

bigstatsr::AUC(x, y) * 5^2

ord <- order(x, y)
x.ord <- x[ord]
y.ord <- y[ord]
rbind(x.ord, y.ord)

# library(dplyr)
# data.frame(
#   x = x,
#   y = y
# ) %>%
#   arrange(desc(x), desc(y))
#
# sum(rank(x)[y == 1] - 1)

latest.control <- x.ord[1] - 2
latest.case <- x.ord[1] - 1
c.control <- 0
c.control.equal <- 0
s.total <- 0
for (i in seq_along(y.ord)) {
  if (y.ord[i] == 0) {
    c.control <- c.control + 1
    # print(latest.control <- x.ord[i])
    if (x.ord[i] == latest.control) {
      c.control.equal <- c.control.equal + 1
    } else {
      latest.control <- x.ord[i]
      c.control.equal <- 0
    }
    # if ((latest.control <- x.ord[i]) == latest.case) s.total <- s.total + 0.5

  } else if (y.ord[i] == 1) {
    if (x.ord[i] == latest.control) {
      s.total <- s.total + c.control - (c.control.equal + 1) / 2
    } else {
      s.total <- s.total + c.control
    }
    # print(latest.case <- x.ord[i])

  } else {
    stop("You shouldn't be here!")
  }
}
s.total
