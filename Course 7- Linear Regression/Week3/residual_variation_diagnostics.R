
data(swiss); par(mfrow = c(2, 2))
fit <- lm(Fertility ~ . , data = swiss); plot(fit)

        ## Influential, high leverage and outlying points
        
n <- 100; x <- rnorm(n); y <- x + rnorm(n, sd = .3)
plot(c(-3, 6), c(-3, 6), type = "n", frame = FALSE, xlab = "X", ylab = "Y")
abline(lm(y ~ x), lwd = 2)
points(x, y, cex = 2, bg = "lightblue", col = "black", pch = 21)
points(0, 0, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(0, 5, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(5, 5, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(5, 0, cex = 2, bg = "darkorange", col = "black", pch = 21)

 
## Case 1
x <- c(10, rnorm(n)); y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))            

dev.off()

## The code

n <- 100; x <- c(10, rnorm(n)); y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))            

## Showing a couple of the diagnostic values
fit <- lm(y ~ x)
round(dfbetas(fit)[1 : 10, 2], 3)
round(hatvalues(fit)[1 : 10], 3)


## Case 2
x <- rnorm(n); y <- x + rnorm(n, sd = .3)
x <- c(5, x); y <- c(5, y)
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
fit2 <- lm(y ~ x)
abline(fit2)            

## Looking at some of the diagnostics
round(dfbetas(fit2)[1 : 10, 2], 3)
round(hatvalues(fit2)[1 : 10], 3)

## Example described by Stefanski TAS 2007 Vol 61.
## Don't everyone hit this server at once.  Read the paper first.
dat <- read.table('http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt', header = FALSE)
pairs(dat)



## Got our P-values, should we bother to do a residual plot?
summary(lm(V1 ~ . -1, data = dat))$coef



## Residual plot
### P-values significant, O RLY?
      
fit <- lm(V1 ~ . - 1, data = dat); plot(predict(fit), resid(fit), pch = '.')

## Back to the Swiss data
       
data(swiss); par(mfrow = c(2, 2))
fit <- lm(Fertility ~ . , data = swiss); plot(fit)

