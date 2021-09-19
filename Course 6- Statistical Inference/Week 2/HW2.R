#1The probability that a manuscript gets accepted to a journal is 12% (say). However, 
#given that a revision is asked for, the probability that it gets accepted is 90%. 
#Is it possible that the probability that a manuscript has a revision asked for is 20%?
#A=accepted, B=revision. P(A)=.12, P(A|B)=.90. P(B)=.20
#P(A∩B)=P(A|B)∗P(B)=.9×.2=.18 this is larger than P(A)=.12, which is not possible since A∩B⊂A.

#2 Suppose that the number of web hits to a particular site are approximately normally 
#distributed with a mean of 100 hits per day and a standard deviation of 10 hits per day.
#What's the probability that a given day has fewer than 93 hits per day expressed as a 
#percentage to the nearest percentage point?
        
round(pnorm(93, mean = 100, sd = 10) * 100)
#3 Suppose 5% of housing projects have issues with asbestos. The sensitivity of a test for 
#asbestos is 93% and the specificity is 88%. What is the probability that a housing project
#has no asbestos given a negative test expressed as a percentage to the nearest percentage point?

#P(Ac|T−)=(P(T−|Ac)P(Ac))/(P(T−|Ac)P(Ac)+P(T−|A)P(A))
(.88 * .95) / (.88 * .95 + .07 * .05)


#4 Suppose that the number of web hits to a particular site are approximately normally distributed 
#with a mean of 100 hits per day and a standard deviation of 10 hits per day.
#What number of web hits per day represents the number so that only 5% of days have more hits?
#Express your answer to 3 decimal places.

round(qnorm(.05, mean = 100, sd = 10, lower.tail = FALSE), 3)


#5 Suppose that the number of web hits to a particular site are approximately normally distributed with 
#a mean of 100 hits per day and a standard deviation of 10 hits per day.
#Imagine taking a random sample of 50 days. What number of web hits would be the point so that only 5% 
#of averages of 50 days of web traffic have more hits? Express your answer to 3 decimal places.

round(qnorm(.05, mean = 100, sd = 10 / sqrt(50), lower.tail = FALSE), 3)

#6 You don't believe that your friend can discern good wine from cheap. Assuming that you're right, 
#in a blind test where you randomize 6 paired varieties (Merlot, Chianti, ...) of cheap and expensive 
#wines
round(pbinom(4, prob = .5, size = 6, lower.tail = FALSE) * 100, 1)


#7 Consider a uniform distribution. If we were to sample 100 draws from a a uniform distribution 
# (which has mean 0.5, and variance 1/12) and take their mean, Xˉ

#What is the approximate probability of getting as large as 0.51 or larger expressed to 3 decimal
#places?

round(pnorm(.51, mean = 0.5, sd = sqrt(1 / 12 / 100), lower.tail = FALSE), 3)


#8 If you roll ten standard dice, take their average, then repeat this process over and
#over and construct a histogram,

# what would it be centered at?
#The answer will be 3.5 since the mean of the sampling distribution of iid draws will 
#be the population mean that the individual draws were taken from.

#9 If you roll ten standard dice, take their average, then repeat #
#this process over and over and construct a histogram,

#what would be its variance expressed to 3 decimal places?
        
#The answer will be 0.292 since the variance of the sampling distribution of the
#mean is σ2/10 where σ2 is the variance of a single die roll, which is

mean((1 : 6 - 3.5)^2 / 10)

#10 The number of web hits to a site is Poisson with mean 16.5 per day.

#What is the probability of getting 20 or fewer in 2 days expressed as a 
#percentage to one decimal place?

round(ppois(20, lambda = 16.5 * 2) * 100, 1)









