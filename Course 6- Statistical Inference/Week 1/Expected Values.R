
#Loading in and displaying the Galton data
library(UsingR); data(galton); library(ggplot2); library(reshape2)
longGalton <- melt(galton, measure.vars = c("child", "parent"))
g <- ggplot(longGalton, aes(x = value)) + geom_histogram(aes(y = ..density..,  fill = variable), binwidth=1, color = "black") + geom_density(size = 2)
g <- g + facet_grid(. ~ variable)
g

library(UsingR); data(galton); library(ggplot2); library(reshape2)
l
#Using manipulate to explore the mean
library(manipulate)
library(ggplot2)
myHist <- function(mu){
        g <- ggplot(galton, aes(x = child))
        g <- g + geom_histogram(fill = "salmon",
                                binwidth=1, aes(y = ..density..), color = "black")
        g <- g + geom_density(size = 2)
        g <- g + geom_vline(xintercept = mu, size = 2)
        mse <- round(mean((galton$child - mu)^2), 3)  
        g <- g + labs(title = paste('mu = ', mu, ' MSE = ', mse))
        g
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))


#plotting binomial variance

p = seq(0 , 1, length = 1000)
y = p * (1 - p)
plot(p, y, type = "l", lwd = 3, frame = FALSE)


#Simulating means of random normals
nosim <- 1000
n <- 10
## simulate nosim averages of 10 standard normals
sd(apply(matrix(rnorm(nosim * n), nosim), 1, mean))

## Let's check to make sure that this is sigma / sqrt(n)
1 / sqrt(n)

#Simulating means of uniforms
nosim <- 1000
n <- 10
sd(apply(matrix(runif(nosim * n), nosim), 1, mean))

1 / sqrt(12 * n)


#Simulating means of Poisson variates
nosim <- 1000
n <- 10
sd(apply(matrix(rpois(nosim * n, 4), nosim), 1, mean))

2 / sqrt(n)


#Simulating means of coin flips
nosim <- 1000
n <- 10
sd(apply(matrix(sample(0 : 1, nosim * n, replace = TRUE),
                  nosim), 1, mean))

 1 / (2 * sqrt(n))

#Data Example

library(UsingR); data(father.son);
x <- father.son$sheight
n<-length(x)
round(c(var(x), var(x) / n, sd(x), sd(x) / sqrt(n)),2)


p <- c(.1,.2,.3,.4)
x <- 2:5
sum(p)
rbind(x,p)
sum(x^2*p)-sum(x*p)^2

#Quiz 1:Question 5:
x <- 1:4
p <- x/sum(x)
temp <- rbind(x,p)
rownames(temp) <- c("X","prob")
temp
sum(p*x)
