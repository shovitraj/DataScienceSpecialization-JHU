#1
#We want to test the hypothesis H0:μ0=0 against Ha:μ0≠0, 
#where μ0 is the difference of the means between the baseline
#and the measures after two weeks.
baseline <- c(140,138,150,148,135)
Week2 <- c(132,135,151,146,130)

t.test(Week2,baseline,alternative="two.sided",paired=TRUE)

#2
#Determine 95% confidenceInterval
CI <- 1100 + c(-1,1)*qt(0.975,8)*30/sqrt(9)
CI


#3
#Let us call p the proportion of people who prefer Coke than Pepsi. 
#Then we want to test the hypothesis H0:p=0.5 versus Ha:p>0.5. 
#It can be done usin the khi squared independance test :
chisq.test(c(3,1),p=c(0.5,0.5))


#4

#We want to test the hypothesis H0:λ=0.01 versus Ha:λ<0.01. We have X=10, t=1787
#and we assume that XH0∼Poisson(λ.t).
lambda <- 0.01
t <- 1787
ppois(10,lambda*t)


#5
#Let us call μdiff,treated and μdiff,placebo the mean values of the difference (followup-baseline) 
#for the treated group and the placebo group. The hypothesis H0 is then :
        
       

# H0:μdiff,treated=μdiff,placebo

n_plac <- 9
n_treat <- 9
mudiff_treat <- -3
mudiff_plac <- 1
sddiff_plac <- 1.8
sddiff_treat <- 1.5

s <- sqrt(((n_plac-1)*sddiff_plac^2 + (n_treat-1)*sddiff_treat^2)/(n_plac + n_treat -2))
t <- (mudiff_treat-mudiff_plac)/(s*sqrt(1/n_plac + 1/n_treat))
2*pt(t,n_plac + n_treat-2)

#6
#mu = 1078 falls in 1077- 1123 range, so you wouldn’t reject the H0.


#7

#Let us call μdiff the mean of the difference of loss (Four weeks - baseline). 
#Then we want to test H0:μdiff=0 versus Ha:μdiff<0.


#The test statistic is t=X¯σ.n‾√. The hypothesis is rejected if t>Z0.95=1.645.
n <- 100
mua <- 0.01
s <- 0.04
alpha <- 0.05
mu0 <- 0              # no brain volum loss

power.t.test(n=n, delta=mua-mu0, sd=s, type="one.sample", alt="one.sided", sig.level=alpha)$power


#8

#We want to test the null hypothesis H0:μdiff=0 against Ha:μdiff≠0, 
#where μdiff is the brain volume loss.
mua <- 0.01
s <- 0.04
power <- 0.9
alpha <- 0.05
mu0 <- 0

power.t.test(power=power, delta=mua-mu0, sd=s, type="one.sample", alt="one.sided", sig.level=alpha)$n


#9
#Power increases as you increase alpha.


