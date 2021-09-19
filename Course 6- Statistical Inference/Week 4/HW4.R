#1
library(datasets)
data(mtcars)
head(mtcars)
mean(mtcars$mpg)
mn <- mean(mtcars$mpg)
s <- sd(mtcars$mpg)
z <- qnorm(.05)
mu0 <- mn - z * s / sqrt(nrow(mtcars))

mu0

#2

m4 <- mtcars$mpg[mtcars$cyl == 4]
m6 <- mtcars$mpg[mtcars$cyl == 6]
p <- t.test(m4, m6, paired = FALSE, alternative="two.sided", var.equal=FALSE)$p.value
m4
m6
p
#The answer to 1. is 1
#The answer to 2. is 4e-04


#4
ans <- round(pbinom(54, prob = .5, size = 100, lower.tail = FALSE),4)
ans
#The answer to 1 is 0.1841
#The answer to 2 is 0

#5
pv <- ppois(15800 - 1, lambda = 520 * 30, lower.tail = FALSE)
pv
#The answer to 1 is 0.0553
#The answer to 2 is 0

#Also, compare with the Gaussian approximation where λˆ∼N(λ,λ/t)
pnorm(15800 / 30, mean = 520, sd = sqrt(520 / 30), lower.tail = FALSE)
#[1] 0.05466
#As t→∞ this becomes more Gaussian. The approximation is pretty good for this setting.

#6
m1 <- 10; m2 <- 11
n1 <- n2 <- 100
s <- 4
se <- s * sqrt(1 / n1 + 1 / n2)
ts <- (m2 - m1) / se
pv <- 2 * pnorm(-abs(ts))
pv

#7

#8
power <- pnorm(10 + qnorm(.95) * .4, mean = 11, sd = .4, lower.tail = FALSE)
power
#The answer is 0.804

#9

n <- (qnorm(.95) + qnorm(.8)) ^ 2 * .04 ^ 2 / .01^2
n
#The answer is 99

#10

#11
mpg8 <- mtcars$mpg[mtcars$cyl == 8]
mpg6<- mtcars$mpg[mtcars$cyl == 6]
p <- t.test(mpg8, mpg6, paired = FALSE, alternative="two.sided", var.equal=TRUE)$p.value
mixprob <- (n8 - 1) / (n8 + n6 - 2)
s <- sqrt(mixprob * s8 ^ 2  +  (1 - mixprob) * s6 ^ 2)
z <- (m8 - m6) / (s * sqrt(1 / n8 + 1 / n6))
pz <- 2 * pnorm(-abs(z))









OK
