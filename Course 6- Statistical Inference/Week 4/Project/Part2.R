library(datasets)
library(ggplot2)
data(ToothGrowth)
str(ToothGrowth)
head(ToothGrowth)
sum(is.na(ToothGrowth))
summary(ToothGrowth)

#Exploratory Analysis


ggplot(aes(x=dose, y = len), data = ToothGrowth) + 
        geom_point(aes(color = supp)) 
# split of cases between different dose levels and delivery methods
table(ToothGrowth$dose, ToothGrowth$supp)

ToothGrowth$dose <- as.factor(ToothGrowth$dose)
g1 <- ggplot(ToothGrowth, aes(x=dose, y=len, fill=dose)) + 
        geom_boxplot() +
        ggtitle("Dispersion of tooth growth by dose") +
        xlab("dose in mg") +
        ylab("tooth length")
g1


g2 <- ggplot(ToothGrowth, aes(x=supp, y=len, fill=supp)) + 
        geom_boxplot() +
        ggtitle("Dispersion of tooth growth by delivery method") +
        xlab("delivery method") +
        ylab("tooth length")
g2

ggplot(aes(x = supp, y = len), data = ToothGrowth) +
        geom_boxplot(aes(fill = supp)) + facet_wrap(~ dose)

#Summary of data within each combination of dose level and delivery method:
by(ToothGrowth$len, INDICES = list(ToothGrowth$supp, ToothGrowth$dose), summary)


#Number of data points within each combination:
length(ToothGrowth$len)

by(ToothGrowth$len, INDICES = list(ToothGrowth$supp, ToothGrowth$dose), length)

#Supplement as a Factor
#Analyzing the data for correlation between the delivery method and change in tooth growth:
        
t.test(len ~ supp, paired = F, var.equal = F, data = ToothGrowth)

#A confidence interval of [-0.171, 7.571] does not allow us to reject the null hypothesis
#(that there is no correlation between delivery method and tooth length).
#Dosage as a Factor
#Analyzing the data for correlation between the dose level and change in tooth growth:
        
dose1 <- subset(ToothGrowth, dose %in% c(0.5, 1.0))
dose2 <- subset(ToothGrowth, dose %in% c(0.5, 2.0))
dose3 <- subset(ToothGrowth, dose %in% c(1.0, 2.0))
t.test(len ~ dose, paired = F, var.equal = F, data = dose1)

t.test(len ~ dose, paired = F, var.equal = F, data = dose2)

t.test(len ~ dose, paired = F, var.equal = F, data = dose3)

#The confidence intervals ([-11.98, -6.276] for doses 0.5 and 1.0, [-18.16, -12.83] for doses 0.5 and 2.0,
#and [-8.996, -3.734] for doses 1.0 and 2.0) allow for the rejection of the null hypothesis and a confirmation that there is a significant correlation between tooth length and dose levels.


