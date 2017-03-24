p <- plot(svd, type = "scores") + geom_qq()

plotly::ggplotly(p + geom_point(data.frame(color = pop), aes(color = pop)),
                 tooltip = "pop")


p2 <- qplot(y = svd$d)
plotly::ggplotly()

p3 <- plotly::plotly_build(p2)
str(p3$x$data[[1]]$text <- )
