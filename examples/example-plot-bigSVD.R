set.seed(1)

test <- big_attachExtdata()
svd <- big_SVD(test, big_scale(), k = 10)

# screeplots
plot(svd) # 3 PCs seems "significant"
plot(svd, coeff = 1.2) # larger font for papers

# scores plot
plot(svd, type = "scores") # first 2 PCs
plot(svd, type = "scores", scores = c(1, 3))
## add color (recall that this return a `ggplot2` object)
class(obj <- plot(svd, type = "scores"))

pop <- rep(c("POP1", "POP2", "POP3"), c(143, 167, 207))
library(ggplot2)
obj$layers[[1]] <- NULL # remove first layer of points
obj
print(obj2 <- obj + geom_point(aes(color = pop)) + labs(color = "Population"))
## change the place of the legend
print(obj3 <- obj2 + theme(legend.position = c(0.85, 0.17)))
## change the title and the labels of the axes
obj3 + ggtitle("Yet another title") + xlab("with an other 'x' label")

# loadings
plot(svd, type = "loadings", loadings = 2)
## all loadings
plot(svd, type = "loadings", loadings = 1:10, coeff = 0.5)

# dynamic plots, require the package **plotly**
\dontrun{plotly::ggplotly(obj3)}
