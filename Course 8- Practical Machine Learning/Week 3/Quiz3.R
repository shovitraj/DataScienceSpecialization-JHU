#1

library(AppliedPredictiveModeling)
data("segmentationOriginal")
library(caret)
library(rattle)


str(segmentationOriginal)
inTrain <- createDataPartition(y= segmentationOriginal$Case, p =0.7,
                               list=FALSE)
training <- segmentationOriginal[inTrain,]
testSet <- segmentationOriginal[-inTrain,]

set.seed(125)

modFit <- train(Class ~., method="rpart", data=training)


modFit$finalModel
fancyRpartPlot(modFit$finalModel)

#2
#If K is small in a K-fold cross validation is the bias in the estimate of out-of-sample (test set) accuracy smaller or bigger?
#If K is small is the variance in the estimate of out-of-sample (test set) accuracy smaller or bigger? Is K large or small in leave one out cross validation ?
        
#The bias is larger and the variance is smaller. Under leave one out cross validation K is equal to the sample size.
#The bias is larger and the variance is smaller. Under leave one out cross validation K is equal to one.
#The bias is smaller and the variance is bigger. Under leave one out cross validation K is equal to one.
#The bias is smaller and the variance is smaller. Under leave one out cross validation K is equal to the sample size.
#Answer : The bias is larger and the variance is smaller. Under leave one out cross validation K is equal to the sample size.

#3
library(pgmm)
rm(list=ls())
data(olive)
olive
olive = olive[, -1]
olive
newdata = as.data.frame(t(colMeans(olive)))
table(olive$Area)
olive_rpart <- train(Area~.,data=olive,method="rpart")
fancyRpartPlot(olive_rpart$finalModel)
predict(olive_rpart, newdata=newdata)

#Answer : : the correct answer is : 2.783. It is strange because Area should be a qualitative variable - but tree is reporting the average value of Area as a numeric variable
#in the leaf predicted for newdata.


#4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1], size = dim(SAheart)[1] / 2, replace = F)
trainSA = SAheart[train, ]
testSA = SAheart[-train, ]

missClass = function(values, prediction){sum(((prediction > 0.5) * 1) != values) / length(values)}


set.seed(13234)
regModel <- train(chd~age+alcohol+obesity+tobacco+typea+ldl,data=trainSA,method="glm",family="binomial")
missClassTrain <- missClass(trainSA$chd,predict(regModel,newdata=trainSA))
missClassTest <- missClass(testSA$chd,predict(regModel,newdata=testSA))
missClassTrain

missClassTest

#Hence, the misclassification rate on the training set is 0.27 and the misclassification rate on the test set is 0.31.


#5
library(ElemStatLearn)
library(randomForest)
data(vowel.train)
data(vowel.test) 


set.seed(33833)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
modelRF <- randomForest(y~.,data=vowel.train)
order(varImp(modelRF),decreasing=TRUE)











