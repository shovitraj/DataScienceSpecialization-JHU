#1
m <- 1100; p <- 0.95; s <- 30; n <- 9; 
c <- m + c(-1,1) * qt(p + (1-p)/2, n-1) * s/sqrt(n)
c



#2

#upper_endpoint = (followup - baseline) + qt(p+(1-p)/2, n-1) * sd / sqrt(n)
#==> sd = (upper_endpoint - (followup - baseline)) * sqrt(n) / qt(p+(1-p)/2, n-1)
upper_endpoint <- 0
followup_less_baseline <- -2
p <- .95
n <- 9
sd <- (upper_endpoint - followup_less_baseline) * sqrt(n) / qt(p+(1-p)/2, n-1)
sd


#3
#Ans: A paired interval
#You could use either
#It’s necessary to use both
#Independent groups, since all subjects were seen under both systems

#4
nnew <- 10
mnew <- 3
varnew <- .6

nold <- 10
mold <- 5
varold <- .68

p <- .95

pooled_variance <- ((nnew-1)*varnew + (nold-1)*varold) / (nnew + nold - 2)
ci <- (mnew - mold) + c(-1, 1) * qt(p + (1-p)/2, (nnew+nold-2)) * sqrt(pooled_variance) * sqrt(1/nnew + 1/nold)
ci


#5
#The interval will be the same width, but shifted.
#Ans:The interval will be narrower.
#The interval will be wider
#It is impossible to tell.

#6

quantile = 0.975 # is 95% with 2.5% on both sides of the range

n_y <- 100 # nights new system
n_x <- 100 # nights old system
s_y <- 0.50# σ new 
s_x <- 2# σ old 
mu_y <- 4# average hours new system
mu_x <- 6# average hours old system

# calculate pooled standard deviation
s_p <- sqrt(((n_x - 1) * s_x^2 + (n_y - 1) * s_y^2)/(n_x + n_y - 2))

confidenceInterval <-  mu_x - mu_y + c(-1, 1) * qnorm(quantile) * s_p * (1 / n_x + 1 / n_y)^.5
round(confidenceInterval,2)

#When subtracting (old - new) the interval is entirely above zero. The new system does not appear to be effective.
#When subtracting (old - new) the interval contains 0. There is not evidence suggesting that the new system is effective.
#Ans: When subtracting (old - new) the interval is entirely above zero. The new system appears to be effective.
#When subtracting (old - new) the interval contains 0. The new system appears to be effective.


#7

quantile = 0.95 # is 90% with 5% on both sides of the range

n_y <- 9 # subjects treated
n_x <- 9 # subjects placebo
s_y <- 1.5# kg/m2 std.dev. treated 
s_x <- 1.8# kg/m2 std.dev. placebo 
mu_y <- -3#  kg/m2 average difference treated
mu_x <- 1#  kg/m2 average difference placebo

# calculate pooled standard deviation
s_p <- sqrt(((n_x - 1) * s_x^2 + (n_y - 1) * s_y^2)/(n_x + n_y - 2))

confidenceInterval <-  mu_y - mu_x + c(-1, 1) * qt(quantile, df=n_y+n_x-2) * s_p * (1 / n_x + 1 / n_y)^.5
round(confidenceInterval,3)



