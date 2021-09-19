library(ggplot2)

time <- seq(0,0.9, by=0.1)
Position<- c(0,0.02,0.06, 0.115, 0.19, 0.29, 0.4, 0.53, 0.68, 0.84)
Velocity <- c(0,0.3,0.475, 0.65, 0.875, 1.05, 1.2, 1.4, 1.55,NA)


data <- data.frame(time, Position, Velocity)
head(data)
data
fit <- lm(Position~time, data)
plot(data$time, data$Position,main="Position vs. Time", xlab="Time", ylab="Position" )


fit2<- lm(Velocity~time, data)
plot(data$time, data$Velocity, main="Velocity vs. Time", xlab="Time", ylab="Velocity")
abline(fit2)
summary(fit2)$coef
ev <- summary(fit2)$coef[1,2]
ev
ea <- summary(fit2)$coef[2,2]
ea
plot(fit2)

write.table(data, "./ErrorAnalysis.csv", sep=" ")



g <- ggplot(data, aes(x=time, y=Velocity))+
        xlab("Time")+
        ylab("Velocity") +
        ggtitle("Velocity vs. time") + 
        geom_point(size = 2, colour = "black", alpha=0.5)+
        geom_smooth(method = "lm", colour = "black")
g