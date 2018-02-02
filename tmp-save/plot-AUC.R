N <- 200
d <- sample(0:1, N, TRUE)
scores_tidy <- data.frame(d = d, Score = 2 * d + rnorm(N))

library(ggplot2)

# Plot scores
bigstatsr:::MY_THEME(
  qplot(Score, fill = as.logical(d), data = scores_tidy,
        geom = "density", alpha = I(0.4)) +
    labs(fill = "Case?")
)

library(plotROC)
bigstatsr:::MY_THEME(
  ggplot(scores_tidy, aes(d = d, m = Score)) +
    style_roc(xlab = "1 - Specificity", ylab = "Sensitivity")
) +
  geom_roc(n.cuts = 0, size = 1) +
  theme(legend.position = c(0.7, 0.3), legend.key.width = unit(4, "line")) +
  coord_equal()
