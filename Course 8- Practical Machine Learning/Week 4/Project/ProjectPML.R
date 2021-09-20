library(knitr)
library(caret)
library(ggplot2)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
#library(corrplot)


#if(!dir.exists("./data")){dir.create("./data")}

url1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
#filepath1 <- "./data/pml_training.csv"
#filepath2 <- "./data/pml_testing.csv"
#download.file (url1, filepath1)
#download.file (url2, filepath2)

training <- read.csv(url(url1), na.strings = c("NA", "#DIV/0!", ""))
testing <- read.csv(url(url2), na.strings= c("NA", "#DIV/0!", ""))
head(testing)
dim(training); dim(testing)
names(training)
head(training); head(testing)
#We first remove those data contains more than 95% of the observation to be NA. We filter out those records.

removeNA <- colSums(is.na(training))/nrow(training) < 0.95
removeNA
training_noNA <- training[, removeNA]
testing_noNA <- testing[,removeNA]
str(training_noNA)

#remove Col1 to 7
training_new <- training_noNA[,-c(1:7)]
testing_new <- testing_noNA[,-c(1:7)]
dim(training_new)
dim(testing_new)



inTrain  <- createDataPartition(training_new$classe, p=0.7, list=FALSE)
TrainSet <- training_new[inTrain, ]
TestSet  <- training_new[-inTrain, ]
dim(TrainSet)

#ML Algorithm- Decision Tree
decisionTree <- train(classe ~., method='rpart', data =TrainSet)
#Predict with decision tree and output the confusion matrix. It seems like the result of the model is not ideal.
decisionTreePred <- predict(decisionTree, TestSet)
decisionTreeCM <- confusionMatrix(TestSet$classe, decisionTreePred)
decisionTreeCM

rpart.plot(decisionTree$finalModel)

#ML Algorithm- Random Forest
#rfMod <- train(classe ~., method='rf', data =TrainSet, prox=TRUE)
#Predict with decision tree and output the confusion matrix. It seems like the result of the model is not ideal.
#rfModPred <- predict(rfMod, TestSet)
#confusionMatrix(TestSet$classe, rfMOdPred)

#rpart.plot(rfMod$finalModel)  

#ML_Gradient Boosting
GBMmod <- train(classe ~ ., method = "gbm", data =TrainSet, verbose = FALSE)
GBMPred <- predict(GBMmod, TestSet)
GBM_CM <- confusionMatrix(TestSet$classe, GBMPred)
GBM_CM
#rpart.plot(GBMmod$finalModel)

#ML Algorithm- Random Forest
rfMod <- train(classe ~., method='rf', data =TrainSet, prox=TRUE)
#Predict with decision tree and output the confusion matrix. It seems like the result of the model is not ideal.
rfModPred <- predict(rfMod, TestSet)
rfCM<- confusionMatrix(TestSet$classe, rfMOdPred)
rfCM
#rpart.plot(rfMod$finalModel)



#Predict Test Set

predict(rfMod, testing_new)