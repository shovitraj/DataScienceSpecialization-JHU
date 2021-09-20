#1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
head(vowel.test)
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

suppressMessages(library(caret))
set.seed(33833)
rfmodel <- suppressMessages(train(y~., data=vowel.train, method="rf"))
gbmmodel <- suppressMessages(train(y~., data=vowel.train, method="gbm"))

rf.result <- predict(rfmodel, vowel.test)
gbm.result <- predict(gbmmodel, vowel.test)

confusionMatrix(vowel.test$y, rf.result)$overall['Accuracy']
confusionMatrix(vowel.test$y, gbm.result)$overall['Accuracy']
idx_agreed <- (rf.result == gbm.result)
confusionMatrix(vowel.test$y[idx_agreed], rf.result[idx_agreed])$overall['Accuracy']

# RF Accuracy = 0.6017
# GBM Accuracy = 0.5087
# Agreement Accuracy = 0.6352
# No choice matches --> chose the closest
#Answer: (by aprox) RF Accuracy = 0.6082 GBM Accuracy = 0.5152 Agreement Accuracy = 0.6361

#2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
rfmodel <- suppressMessages(train(diagnosis~., data=training, method="rf"))
gbmmodel <- suppressMessages(train(diagnosis~., data=training, method="gbm"))
ldamodel <- suppressMessages(train(diagnosis~., data=training, method="lda"))

rfresult <- predict(rfmodel, testing)
gbmresult <- predict(gbmmodel, testing)
ldaresult <- predict(ldamodel, testing)
combined.data <- data.frame(rfresult, gbmresult, ldaresult, diagnosis=testing$diagnosis)
combined.model <- train(diagnosis~., data=combined.data, method="rf")

combined.result <- predict(combined.model, testing)
confusionMatrix(testing$diagnosis, rfresult)$overall['Accuracy']

confusionMatrix(testing$diagnosis, gbmresult)$overall['Accuracy']

confusionMatrix(testing$diagnosis, ldaresult)$overall['Accuracy']

confusionMatrix(testing$diagnosis, combined.result)$overall['Accuracy']

#Stacked Accuracy: 0.80 is better than random forests and lda and the same as boosting.


#3

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]


set.seed(233)
mod_lasso <- train(CompressiveStrength ~ ., data = training, method = "lasso")
library(elasticnet)
plot.enet(mod_lasso$finalModel, xvar = "penalty", use.color = TRUE)

#The coefficient path shows that the variable Cement is the last coefficient to be set to zero as the penalty increases.

#4

library(lubridate)  # For year() function below

url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"
filepath <- "./gaData.csv"
download.file(url, filepath)
dat = read.csv("./gaData.csv")
training = dat[year(dat$date) < 2012, ]
testing = dat[(year(dat$date)) > 2011, ]
tstrain = ts(training$visitsTumblr)

library(forecast)
mod_ts <- bats(tstrain)
fcast <- forecast(mod_ts, level = 95, h = dim(testing)[1])
sum(fcast$lower < testing$visitsTumblr & testing$visitsTumblr < fcast$upper) / 
        dim(testing)[1]

#around 96% of the testing points is the true value within the 95% prediction interval bounds.
#5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[inTrain, ]
testing = concrete[-inTrain, ]
set.seed(325)
library(e1071)
mod_svm <- svm(CompressiveStrength ~ ., data = training)
pred_svm <- predict(mod_svm, testing)
accuracy(pred_svm, testing$CompressiveStrength)

#6.72

