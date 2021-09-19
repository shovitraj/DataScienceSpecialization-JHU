#create matrix with 1000 simulations with 40 samples 
#simd: simulated data
# simmu: simulated mean
#simvar: simulated variance
#simsd: simulared standard deviation
#tmu: theoretical mean
#tvar: theoretical variance
#tsd: theroretical standard deviation
library(ggplot2)
set.seed(554)
lambda <- 0.2
n <- 40
nofsim <- 1000


simd<- matrix( rexp(n * nofsim, rate= lambda), nofsim)
simd
dim(simd)
simmu <- rowMeans(simd)
simmu


#Mean Comparision
sammu <- mean(simmu)
sammu


tmu <- 1/lambda
tmu

round(sammu) == round(tmu)

#Var Comp

samvar <- var(simmu)
samvar


tvar <- (1/lambda)^2/n
tvar

round(samvar) == round(tvar)


samsd <- sd(simmu)
samsd


tsd <- 1/(lambda * sqrt(n))
tsd


round(samsd) == round(tsd)



samCI <- round (mean(simmu) + c(-1,1)*1.96*sd(simmu)/sqrt(n),3)
samCI

tCI<- tmu + c(-1,1) * 1.96 * sqrt(tvar)/sqrt(n)
tCI

qqnorm(simmu, main="Normal Q-Q Plot", xlab="Theoretical Quantiles", ylab="Sample Quantiles")
qqline(simmu, col="red")

hist(simmu, col="darkblue", main="Theoretical vs actual mean for rexp()", breaks=20)
abline(v=mean(sammu), lwd="4", col="red")
text(3.6, 90, paste("Actual mean = ", round(mean(samMean),2), "\n Theoretical mean = 5" ), col="red")

data <- data.frame(simmu)
g <- ggplot(data, aes(x=simmu))
g <- g + geom_histogram(aes(y=..density..), colour="black", fill = "azure3") + 
         labs(title = "Distribution of mean of 40 samples", x ="Mean of 40 samples", y="Density")+
         geom_vline(aes(xintercept = samMean, color ="deeppink")) + 
         geom_vline(aes(xintercept = samMean, color ="cornsilk")) +
         stat_function(fun = dnorm, args = list(mean = samMean, sd = samsd), color = "green", size = 1.0) +
         stat_function(fun = dnorm, args = list(mean = theoMean, sd = tsd), color = "coral", size = 1.0)
g


