library(knitr)
library(caret)
library(ggplot2)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
library(parallel)
library(doParallel)
#library(corrplot)

cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

if(!dir.exists("./data")){dir.create("./data")}

url1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
filepath1 <- "./data/pml_training.csv"
filepath2 <- "./data/pml_testing.csv"
download.file (url1, filepath1)
download.file (url2, filepath2)

training <- read.csv("./data/pml_training.csv", na.strings = c("NA", "#DIV/0!", ""))
testing <- read.csv("./data/pml_testing.csv", na.strings= c("NA", "#DIV/0!", ""))
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
testing_new <-testing_noNA[,-c(1:7)]
dim(training_new)
dim(testing_new)



inTrain  <- createDataPartition(training_new$classe, p=0.7, list=FALSE)
TrainSet <- training_new[inTrain, ]
TestSet  <- training_new[-inTrain, ]
dim(TrainSet)

dtmodel <-train(classe~., data=TrainSet, method="rpart")
dtpred <- predict(dtmodel, TestSet)
decisionTreeCM <- confusionMatrix(TestSet$classe, dtpred)
decisionTreeCM
rpart.plot(dtmodel$finalModel)
dtAccuracy <- confusionMatrix(TestSet$class, dtpred)$overall['Accuracy']
dtAccuracy

gbmmodel <-train(classe~., data=TrainSet, method="gbm")
gbmpred<- predict(gbmmodel, TestSet)
GBM_CM <- confusionMatrix(TestSet$classe, gbmpred)
GBM_CM
gbmAccuracy<- confusionMatrix(TestSet$class, gbmpred)$overall['Accuracy']
gbmAccuracy

#1: Configure Parallel Processing
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
cluster
registerDoParallel(cluster)

#2: Configure trainControl object
fitControl <- trainControl(method = "cv",
                           number = 5,
                           allowParallel = TRUE)

#3: develop rf training model
rfmodel <- train(classe~., data=TrainSet, method="rf", trControl=fitControl)
#combined.model <- train(classe~., data=combined.data, method="rf", trControl=fitControl)
stopCluster(cluster)
registerDoSEQ()

#4: de-register parallel processing cluster
rfpred <- predict(rfmodel, TestSet)
rfCM<- confusionMatrix(TestSet$classe, rfpred)
rfCM
rfAccuracy<- confusionMatrix(TestSet$class, rfpred)$overall['Accuracy']
rfAccuracy

#combined.data <- data.frame(dtpred, gbmpred, rfpred, classe= TestSet$classe)
#combined.model <- train(classe~., data=combined.data, method="rf")
#combined.pred<- predict(combined.model, TestSet)
#combined.CM <- confusionMatrix(TestSet$classe, combined.pred)
#combined.CM
#combined.Accuracy <- confusionMatrix(TestSet$classe, combined.pred)$overall['Accuracy']
#combined.Accuracy

#Predict Test Set

predict(rfmodel, testing_new)




