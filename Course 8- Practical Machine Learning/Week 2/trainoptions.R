#SPAM Example
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
modelFit <- train(type ~.,data=training, method="glm")


#Train options
args(train.default)
function (x, y, method = "rf", preProcess = NULL, ..., weights = NULL, 
          metric = ifelse(is.factor(y), "Accuracy", "RMSE"), maximize = ifelse(metric == 
                                                                                       "RMSE", FALSE, TRUE), trControl = trainControl(), tuneGrid = NULL, 
          tuneLength = 3) 
        
  #      trainControl
args(trainControl)
function (method = "boot", number = ifelse(method %in% c("cv", 
                                                         "repeatedcv"), 10, 25), repeats = ifelse(method %in% c("cv", 
                                                                                                                "repeatedcv"), 1, number), p = 0.75, initialWindow = NULL, 
          horizon = 1, fixedWindow = TRUE, verboseIter = FALSE, returnData = TRUE, 
          returnResamp = "final", savePredictions = FALSE, classProbs = FALSE, 
          summaryFunction = defaultSummary, selectionFunction = "best", 
          custom = NULL, preProcOptions = list(thresh = 0.95, ICAcomp = 3, 
                                               k = 5), index = NULL, indexOut = NULL, timingSamps = 0, 
          predictionBounds = rep(FALSE, 2), seeds = NA, allowParallel = TRUE) 
        
        
#        seed example
set.seed(1235)
modelFit2 <- train(type ~.,data=training, method="glm")
modelFit2


#seed example
set.seed(1235)
modelFit3 <- train(type ~.,data=training, method="glm")
modelFit3