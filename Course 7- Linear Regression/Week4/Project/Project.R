library(datasets)
library(ggplot2)
library(corrplot)
library(GGally)
library(dplyr)
data(mtcars)
head(mtcars, 5)
str(mtcars)
summary(mtcars$mpg)

# Transform certain variables into factors
mtcars$cyl  <- factor(mtcars$cyl)
mtcars$vs   <- factor(mtcars$vs)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
mtcars$am   <- factor(mtcars$am,labels=c("Automatic","Manual"))

table(mtcars$am)
aggregate(mpg~am, mtcars, mean)

g1 <-ggplot(aes(x=am, y=mpg), data=mtcars) + 
    geom_boxplot(aes(fill=am))+
    xlab("MPG") + ylab("Transmission Type") + ggtitle("Mileage by Transmission") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_discrete(labels=c("Automatic","Manual")) +
    scale_fill_discrete(name="Transmission",labels=c("Automatic", "Manual")) +
    theme(plot.title = element_text(color="blue", size=16, face="bold"))
g1


g2 <-ggplot(aes(x=cyl, y=mpg), data=mtcars) + 
        geom_boxplot(aes(fill=cyl))+
        xlab("Number for Cylinders")+ylab("MPG") + ggtitle("Mileage by Cylinder")+
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_x_discrete(labels=c(4,6,8)) +
        scale_fill_discrete(name="Cylinders",labels=c(4,6,8)) +
        theme(plot.title = element_text(color="blue", size=16, face="bold"))
g2



model1 <- lm(mpg~am, data=mtcars)
summary(model1)

model2 <- lm(mpg~ am + cyl  , data=mtcars)

summary(model2)

anova(model1, model2)

model3 <- lm(mpg~ am + cyl + wt , data=mtcars)

summary(model3)

anova(model1, model3)

model4 <- lm(mpg~ am + cyl + wt + hp , data=mtcars)

summary(model4)

anova(model1, model4)






g3 <- ggpairs(mtcars)
g3

par(mfrow = c(2,2))
plot(model4)

predicted <- predict(model4)

head(mtcars)


