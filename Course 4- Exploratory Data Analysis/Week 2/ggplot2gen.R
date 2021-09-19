
library(ggplot2)
testdat <- data.frame(x = 1:100, y = rnorm(100))
testdat[50,2] <- 100  ## Outlier!
plot(testdat$x, testdat$y, type = "l", ylim = c(-3,3))

g <- ggplot(testdat, aes(x = x, y = y))
g + geom_line()

g + geom_line() + ylim(-3, 3)

g + geom_line() + coord_cartesian(ylim = c(-3, 3))