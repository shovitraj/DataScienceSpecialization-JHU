#Galton Data Histogram

library(UsingR)
data(galton)
par(mfrow=c(1,2))
hist(galton$child, col="blue", breaks=15)
hist(galton$parent, col="blue", breaks=15)
dev.off()

#Use R studio's manipulate to see what value of $\mu$ minimizes the sum of the squared deviations.
library(manipulate)
myHist <- function(mu){
        hist(galton$child,col="blue",breaks=20)
        lines(c(mu, mu), c(0, 150),col="red",lwd=5)
        mse <- mean((galton$child - mu)^2)
        text(63, 150, paste("mu = ", mu))
        text(63, 140, paste("MSE = ", round(mse, 2)))
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))

#The least squares estimate is the empirical mean
hist(galton$child,col="blue",breaks=20)
meanChild <- mean(galton$child)
lines(rep(meanChild,100),seq(0,150,length=100),col="red",lwd=5)

#Comparing childrens' heights and their parents' heights
plot(galton$parent,galton$child,pch=19,col="blue")
#Regression throught the origin
myPlot <- function(beta){
        y <- galton$child - mean(galton$child)
        x <- galton$parent - mean(galton$parent)
        freqData <- as.data.frame(table(x, y))
        names(freqData) <- c("child", "parent", "freq")
        plot(
                as.numeric(as.vector(freqData$parent)), 
                as.numeric(as.vector(freqData$child)),
                pch = 21, col = "black", bg = "lightblue",
                cex = .15 * freqData$freq, 
                xlab = "parent", 
                ylab = "child"
        )
        abline(0, beta, lwd = 3)
        points(0, 0, cex = 2, pch = 19)
        mse <- mean( (y - beta * x)^2 )
        title(paste("beta = ", beta, "mse = ", round(mse, 3)))
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))

lm(I(child - mean(child))~ I(parent - mean(parent)) - 1, data = galton)


#Galton's Data
y <- galton$child
x <- galton$parent
beta1 <- cor(y, x) *  sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
beta1
beta0
rbind(c(beta0, beta1), coef(lm(y ~ x)))


#Reversing the outcome/predictor relationship
beta1 <- cor(y, x) *  sd(x) / sd(y)
beta0 <- mean(x) - beta1 * mean(y)
rbind(c(beta0, beta1), coef(lm(x ~ y)))

#Regression through the origin yields an equivalent slope if you center the data first
yc <- y - mean(y)
xc <- x - mean(x)
beta1 <- sum(yc * xc) / sum(xc ^ 2)
c(beta1, coef(lm(y ~ x))[2])
#Normalizing variables results in the slope being the correlation
yn <- (y - mean(y))/sd(y)
xn <- (x - mean(x))/sd(x)
c(cor(y, x), cor(yn, xn), coef(lm(yn ~ xn))[2])

