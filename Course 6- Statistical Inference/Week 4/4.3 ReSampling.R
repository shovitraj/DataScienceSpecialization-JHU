#We want to estimate the bias and standard error of the median
library(UsingR)
data(father.son)
x <- father.son$sheight
n <- length(x)
theta <- median(x)
jk <- sapply(1 : n,
             function(i) median(x[-i])
)
thetaBar <- mean(jk)
biasEst <- (n - 1) * (thetaBar - theta) 
seEst <- sqrt((n - 1) * mean((jk - thetaBar)^2))

c(biasEst, seEst)

library(bootstrap)
temp <- jackknife(x, median)
c(temp$jack.bias, temp$jack.se)

B <- 1000
resamples <- matrix(sample(x,
                           n * B,
                           replace = TRUE),
                    B, n)
medians <- apply(resamples, 1, median)
sd(medians)

quantile(medians, c(.025, .975))

hist(medians)

data(InsectSprays)
boxplot(count ~ spray, data = InsectSprays)

subdata <- InsectSprays[InsectSprays$spray %in% c("B", "C"),]
y <- subdata$count
group <- as.character(subdata$spray)
testStat <- function(w, g) mean(w[g == "B"]) - mean(w[g == "C"])
observedS
tat <- testStat(y, group)
permutations <- sapply(1 : 10000, function(i) testStat(y, sample(group)))
observedStat

mean(permutations > observedStat)















