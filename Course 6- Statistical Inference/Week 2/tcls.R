#Confidence interval for the standard deviation of sons' heights from Galton's data
library(UsingR)
data(father.son)
x <- father.son$sheight
s <- sd(x)
n <- length(x)
round(sqrt((n - 1) * s^2/qchisq(c(0.975, 0.025), n - 1)), 3)

#The data
#Confidence Interval

data(sleep)
head(sleep)

g1 <- sleep$extra[1:10]
g2 <- sleep$extra[11:20]
difference <- g2 - g1
mn <- mean(difference)
s <- sd(difference)
n <- 10
mn + c(-1, 1) * qt(0.975, n - 1) * s/sqrt(n)
t.test(difference)$conf.int

#Plotting Likelihood
pvals <- seq(0, 1, length = 1000)
plot(pvals, dbinom(3, 4, pvals) / dbinom(3, 4, 3/4), type = "l", frame = FALSE, lwd = 3, xlab = "p", ylab = "likelihood / max likelihood")


#Example

#You saw 5 failure events per 94 days of monitoring a nuclear pump.
#Assuming Poisson, plot the likelihood
lambda <- seq(0, .2, length = 1000)
likelihood <- dpois(5, 94 * lambda) / dpois(5, 5)
plot(lambda, likelihood, frame = FALSE, lwd = 3, type = "l", xlab = expression(lambda))
lines(rep(5/94, 2), 0 : 1, col = "red", lwd = 3)
lines(range(lambda[likelihood > 1/16]), rep(1/16, 2), lwd = 2)
lines(range(lambda[likelihood > 1/8]), rep(1/8, 2), lwd = 2)

## Exploring the beta density
library(manipulate)
pvals <- seq(0.01, 0.99, length = 1000)
manipulate(
        plot(pvals, dbeta(pvals, alpha, beta), type = "l", lwd = 3, frame = FALSE),
        alpha = slider(0.01, 10, initial = 1, step = .5),
        beta = slider(0.01, 10, initial = 1, step = .5)
)

pvals <- seq(0.01, 0.99, length = 1000)
x <- 13; n <- 20
myPlot <- function(alpha, beta){
        plot(0 : 1, 0 : 1, type = "n", xlab = "p", ylab = "", frame = FALSE)
        lines(pvals, dbeta(pvals, alpha, beta) / max(dbeta(pvals, alpha, beta)), 
              lwd = 3, col = "darkred")
        lines(pvals, dbinom(x,n,pvals) / dbinom(x,n,x/n), lwd = 3, col = "darkblue")
        lines(pvals, dbeta(pvals, alpha+x, beta+(n-x)) / max(dbeta(pvals, alpha+x, beta+(n-x))),
              lwd = 3, col = "darkgreen")
        title("red=prior,green=posterior,blue=likelihood")
}
manipulate(
        myPlot(alpha, beta),
        alpha = slider(0.01, 10, initial = 1, step = .5),
        beta = slider(0.01, 10, initial = 1, step = .5)
)

#Getting HPD intervals for this example
#Install the \texttt{binom} package, then the command
library(binom)
binom.bayes(13, 20, type = "highest")

pvals <- seq(0.01, 0.99, length = 1000)
x <- 13; n <- 20
myPlot2 <- function(alpha, beta, cl){
        plot(pvals, dbeta(pvals, alpha+x, beta+(n-x)), type = "l", lwd = 3,
             xlab = "p", ylab = "", frame = FALSE)
        out <- binom.bayes(x, n, type = "highest", 
                           prior.shape1 = alpha, 
                           prior.shape2 = beta, 
                           conf.level = cl)
        p1 <- out$lower; p2 <- out$upper
        lines(c(p1, p1, p2, p2), c(0, dbeta(c(p1, p2), alpha+x, beta+(n-x)), 0), 
              type = "l", lwd = 3, col = "darkred")
}
manipulate(
        myPlot2(alpha, beta, cl),
        alpha = slider(0.01, 10, initial = 1, step = .5),
        beta = slider(0.01, 10, initial = 1, step = .5),
        cl = slider(0.01, 0.99, initial = 0.95, step = .01)
)

