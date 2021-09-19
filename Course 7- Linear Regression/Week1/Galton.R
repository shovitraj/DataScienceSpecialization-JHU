#Exploratory Analysis of Galton's Data

library(ggplot2)
library(UsingR); data(galton); library(reshape)
head(galton)

long <- melt(galton)
head(long)
g <- ggplot(long, aes(x = value, fill = variable))
g <- g + geom_histogram(colour = "black", binwidth=1)
g <- g + facet_grid(. ~ variable)
g

#Using manipulate to find the least squares estimate

library(manipulate)
myHist <- function(mu){
        mse <- mean((galton$child-mu)^2)
        g <- ggplot(galton, aes(x = child)) +
             geom_histogram(fill = "salmon", colour = "black", binwidth=1)+
             geom_vline(xintercept = mu, size = 3)+ 
             ggtitle(paste("mu = ", mu, ", MSE = ", round(mse, 2), sep = ""))
        g
}
manipulate(myHist(mu), mu=slider(62,74, step=0.5))

#comparing children's heights and their parents height

g1 <- ggplot(galton, aes(x=parent, y=child)) +
      geom_point()
g1

#regression through the origin
#code for plotting the data
library(dplyr)
y <- galton$child- mean(galton$child)
x <- galton$parent - mean(galton$parent)
freqData <- as.data.frame(table(x,y))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
myPlot <- function(beta){
        g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
        g <- g + scale_size(range = c(2, 20), guide = "none" )
        g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
        g <- g + geom_point(aes(colour=freq, size = freq))
        g <- g + scale_colour_gradient(low = "lightblue", high="white")
        g <- g + geom_abline(intercept = 0, slope = beta, size = 3)
        mse <- mean( (y - beta * x) ^2 )
        g <- g + ggtitle(paste("beta = ", beta, "mse = ", round(mse, 3)))
        g
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))

#the solution
lm(I(child - mean(child))~ I(parent - mean(parent)) - 1, data = galton)
lm(formula = I(child - mean(child)) ~ I(parent - mean(parent)) -
           1, data = galton)

#Fitting Galton's data using linear y <- galton$child
y <- galton$child
x <- galton$parent
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
rbind(c(beta0, beta1), coef(lm(y ~ x))) 
#We can see that the result of lm is identical to hard coding the fit ourselves. Let’s Reversing the
#outcome/predictor relationship
beta1 <- cor(y, x) * sd(x) / sd(y)
beta0 <- mean(x) - beta1 * mean(y)
rbind(c(beta0, beta1), coef(lm(x ~ y)))

#Now let’s show that regression through the origin yields an equivalent slope if you center the data
#first
 yc <- y - mean(y)
 xc <- x - mean(x)
 beta1 <- sum(yc * xc) / sum(xc ^ 2)
c(beta1, coef(lm(y ~ x))[2])
#Now let’s show that normalizing variables results in the slope being the correlation
yn <- (y - mean(y))/sd(y)
xn <- (x - mean(x))/sd(x)
c(cor(y, x), cor(yn, xn), coef(lm(yn ~ xn))[2])

#Regression to the mean
library(UsingR)
data(father.son)
y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
rho <- cor(x, y)
library(ggplot2)
g = ggplot(data.frame(x, y), aes(x = x, y = y))
g = g + geom_point(size = 5, alpha = .2, colour = "black")
g = g + geom_point(size = 4, alpha = .2, colour = "red")
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
g = g + geom_abline(position = "identity")
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2)
g = g + xlab("Father's height, normalized")
g = g + ylab("Son's height, normalized")
g


