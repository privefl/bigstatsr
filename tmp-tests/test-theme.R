library(ggplot2)
library(bigstatsr)

theme_bigstatsr <- function(size.rel = 1) {
  theme_bw() +
    theme(plot.title    = element_text(size = rel(2.0 * size.rel), hjust = 0.5),
          plot.subtitle = element_text(size = rel(1.5 * size.rel), hjust = 0.5),
          legend.title  = element_text(size = rel(1.8 * size.rel)),
          legend.text   = element_text(size = rel(1.3 * size.rel)),
          axis.title    = element_text(size = rel(1.5 * size.rel)),
          axis.text     = element_text(size = rel(1.2 * size.rel)),
          legend.key.height = unit(1.3 * size.rel, "line"),
          legend.key.width  = unit(1.3 * size.rel, "line"))
}

qplot(y = 1:10) +
  ggtitle("Test") +
  theme_bigstatsr()

size.rel <- 1
qplot(y = 1:10) +
  ggtitle("Test") +
  theme_bw() +
  theme(plot.title    = element_text(size = rel(2.0 * size.rel), hjust = 0.5),
        plot.subtitle = element_text(size = rel(1.5 * size.rel), hjust = 0.5),
        legend.title  = element_text(size = rel(1.8 * size.rel)),
        legend.text   = element_text(size = rel(1.3 * size.rel)),
        axis.title    = element_text(size = rel(1.5 * size.rel)),
        axis.text     = element_text(size = rel(1.2 * size.rel)),
        legend.key.height = unit(1.3 * size.rel, "line"),
        legend.key.width  = unit(1.3 * size.rel, "line"))
