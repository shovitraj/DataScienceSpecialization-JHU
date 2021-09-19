#Law of large numbers in action

n <- 10000; means <- cumsum(rnorm(n)) / (1  : n)
plot(1 : n, means, type = "l", lwd = 2, 
     frame = FALSE, ylab = "cumulative means", xlab = "sample size")
abline(h = 0)

#Give a confidence interval for the average height of sons
#in Galton's data

library(UsingR);data(father.son); x <- father.son$sheight
(mean(x) + c(-1, 1) * qnorm(.975) * sd(x) / sqrt(length(x))) / 12

round(1 / sqrt(10 ^ (1 : 6)), 3)


#Poisson Interval

x <- 5; t <- 94.32; lambda <- x / t
round(lambda + c(-1, 1) * qnorm(.975) * sqrt(lambda / t), 3)

poisson.test(x, T = 94.32)$conf

#In the regression class

exp(confint(glm(x ~ 1 + offset(log(t)), family = poisson(link = log))))
