#Plotting all possible likelihoods for a small n
n <- 5
pvals <- seq(0, 1, length = 1000)
plot(c(0, 1), c(0, 1.2), type = "n", frame = FALSE, xlab = "p", ylab = "likelihood")
text((0 : n) /n, 1.1, as.character(0 : n))
sapply(0 : n, function(x) {
        phat <- x / n
        if (x == 0) lines(pvals,  ( (1 - pvals) / (1 - phat) )^(n-x), lwd = 3)
        else if (x == n) lines(pvals, (pvals / phat) ^ x, lwd = 3)
        else lines(pvals, (pvals / phat ) ^ x * ( (1 - pvals) / (1 - phat) ) ^ (n-x), lwd = 3) 
}
)
title(paste("Likelihoods for n = ", n))

choose(8, 7) * .5 ^ 8 + choose(8, 8) * .5 ^ 8 

pbinom(6, size = 8, prob = .5, lower.tail = FALSE)

plot(pvals, dbinom(7, 8, pvals) / dbinom(7, 8, 7/8) , 
     lwd = 3, frame = FALSE, type = "l", xlab = "p", ylab = "likelihood")

#The normal distribution
zvals <- seq(-3, 3, length = 1000)
plot(zvals, dnorm(zvals), 
     type = "l", lwd = 3, frame = FALSE, xlab = "z", ylab = "Density")
sapply(-3 : 3, function(k) abline(v = k))

#Poisson 
ppois(3, lambda = 2.5 * 4)

#Example, Poisson approximation to binomial
pbinom(2, size = 500, prob = .01)

ppois(2, lambda=500 * .01)

pbinom(2, size = 500, prob = .01)

