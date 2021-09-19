
#lapply
x <- list(a = 1:5, b= rnorm(10))
lapply(x, mean)

x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5))
lapply(x, mean)

x <- 1:4
lapply(x, runif)


x <- 1:4
lapply(x, runif, min = 0, max = 10)

x <- list(a = matrix(1:4, 2, 2), b = matrix(1:6, 3, 2))
x
#Extract the first column of each matrix
lapply(x, function(elt) { elt[,1] })

f <- function(elt) {
        elt[, 1]
        }
lapply(x, f)

#sapply

x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5))
lapply(x, mean)
sapply(x, mean)

#split()
str(split)

x <- c(rnorm(10), runif(10), rnorm(10, 1))
f <- gl(3, 10)
split(x, f)

lapply(split(x, f), mean)

#splitting dataframes

library(datasets)
head(airquality)
s <- split(airquality, airquality$Month)
str(s)

lapply(s, function(x) {
        colMeans(x[, c("Ozone", "Solar.R", "Wind")])
        })

sapply(s, function(x) {
        colMeans(x[, c("Ozone", "Solar.R", "Wind")])
        })

sapply(s, function(x) {
        colMeans(x[, c("Ozone", "Solar.R", "Wind")],
                   na.rm = TRUE)
         })

x <- rnorm(10)
f1 <- gl(2, 5)
f2 <- gl(5, 2)
f1
f2
interaction(f1, f2)

str(split(x, list(f1, f2)))

str(split(x, list(f1, f2), drop = TRUE))


#tapply
str(tapply)
x <- c(rnorm(10), runif(10), rnorm(10, 1))
f <- gl(3, 10)
f
tapply(x, f, mean)
tapply(x, f, mean, simplify = FALSE)
tapply(x, f, range)

#apply

str(apply)
x <- matrix(rnorm(200), 20, 10)
apply(x, 2, mean)
apply(x, 1, sum)
apply(x, 2, mean)
apply(x, 1, mean)

x <- matrix(rnorm(200), 20, 10)
 ## Get row quantiles
apply(x, 1, quantile, probs = c(0.25, 0.75))

a <- array(rnorm(2 * 2 * 10), c(2, 2, 10))
apply(a, c(1, 2), mean)

rowMeans(a, dims = 2)

#mapply()

str(mapply)
mapply(rep, 1:4, 4:1)
noise <- function(n, mean, sd) {
        rnorm(n, mean, sd)
}

noise(5, 1, 2)
noise(1:5, 1:5,2)
mapply(noise, 1:5, 1:5, 2)

list(noise(1, 1, 2), noise(2, 2, 2),
     noise(3, 3, 2), noise(4, 4, 2),
     noise(5, 5, 2))

#Vectorizing a function
sumsq <- function(mu, sigma, x) {
        sum(((x - mu) / sigma)^2)
}

x <- rnorm(100) ## Generate some data
sumsq(1:10, 1:10, x)

mapply(sumsq, 1:10, 1:10, MoreArgs = list(x = x))

vsumsq <- Vectorize(sumsq, c("mu", "sigma"))
vsumsq(1:10, 1:10, x)

#Debugging
log(-1)
printmessage <- function(x) {
        if(x > 0)
                print("x is greater than zero")
        else
                 print("x is less than or equal to zero")
        invisible(x)
}

printmessage(1)
printmessage(NA)

printmessage2 <- function(x) {
        if(is.na(x))
                 print("x is a missing value!")
         else if(x > 0)
                print("x is greater than zero")
        else
                print("x is less than or equal to zero")
        invisible(x)
}

printmessage2(NA)

x <- log(c(-1, 2))

printmessage2(x)

printmessage3 <- function(x) {
        if(length(x) > 1L)
                stop("'x' has length > 1")
        if(is.na(x))
                print("x is a missing value!")
        else if(x > 0)
                print("x is greater than zero")
        else
                print("x is less than or equal to zero")
        invisible(x)
}

printmessage3(1:2)


printmessage4 <- Vectorize(printmessage2)
out <- printmessage4(c(-1, 2))
#traceback
mean(x)
traceback()

lm(y ~ x)
traceback()
#debug
debug(lm)
lm(y~x)

#recover()

options(error=recover)
read.csv("nosuchfile")


