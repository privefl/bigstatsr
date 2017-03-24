require(bigstatsr)


data <- data.frame(Populaion = pop)
paste(names(data), data, sep = ": ")


plotly::ggplotly(p2, tooltip = c("all")) %>%
  add_text(text = paste("Population:", pop), visible = FALSE)
plotly::ggplotly(p2, tooltip = c("all")) %>% add_trace(text = "lla")


ind <- seq_along(pop)


p4 <- plotly::ggplotly(p + geom_point(aes(color = pop,
),
tooltip = "pop")
p3 <- plotly::plotly_build(p2)
str(p3$x$data[[2]]$text)

require(plotly)
p4 %>% add_markers()
p4 %>% add_lines()
p4 %>% add_text(text = ".")


set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]

p <- ggplot(data = d, aes(x = carat, y = price)) +
  geom_point(aes(text = paste("Clarity:", clarity)), size = .5) +
  geom_smooth(aes(colour = cut, fill = cut)) + facet_wrap(~ cut)

p5 <- ggplotly(p)
