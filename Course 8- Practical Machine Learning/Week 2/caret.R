library(caret);
library(kernlab); 
data(spam)
str(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
inTrain
head(inTrain)
dim(spam)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)
dim(testing)
training



#Fit a Model
set.seed(32343)
modelFit <- train(type ~.,data=training, method="glm")
modelFit

#Fit Final Model
modelFit <- train(type ~.,data=training, method="glm")
modelFit$finalModel
#Prediction
predictions <- predict(modelFit,newdata=testing)
predictions  

confusionMatrix(predictions, testing$type)

