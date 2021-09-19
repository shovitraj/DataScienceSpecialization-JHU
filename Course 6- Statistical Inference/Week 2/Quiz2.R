# 1

print("Var(X) = sigma^2 / n")

#2
prob <- pnorm(70, mean = 80, sd = 10)
paste(round(prob * 100), '%', sep = "")


#3
volume <- qnorm(0.95, mean = 1100, sd = 75)
paste("approximately", round(volume), sep = " ")

#4
volume <- qnorm(0.95, mean = 1100, sd = 75/sqrt(100))
paste("approximately", round(volume), sep = " ")
#5

prob <- pbinom(3, size = 5, prob = 0.5, lower.tail = FALSE)
paste(round(prob * 100), '%', sep = "")
# Alternative Method
prob <- choose(5,4) * .5 ^ 5 + choose(5,5) * .5 ^ 5
paste(round(prob * 100), '%', sep = "")


#6
prob <- pnorm(16, mean = 15, sd = 10/sqrt(100)) - pnorm(14, mean = 15, sd = 10/sqrt(100))
paste(round(prob * 100), '%', sep = "")


#7
print("0.5") # Because of LLN


#8

prob <- ppois(10, lambda = 15)
print(round(prob, 2))




