#Ozone data
library(caret)
library(ElemStatLearn); data(ozone,package="ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)

#Bagged loess
ll <- matrix(NA,nrow=10,ncol=155)
for(i in 1:10){
        ss <- sample(1:dim(ozone)[1],replace=T)
        ozone0 <- ozone[ss,]; ozone0 <- ozone0[order(ozone0$ozone),]
        loess0 <- loess(temperature ~ ozone,data=ozone0,span=0.2)
        ll[i,] <- predict(loess0,newdata=data.frame(ozone=1:155))
}

#Bagged loess
plot(ozone$ozone,ozone$temperature,pch=19,cex=0.5)
for(i in 1:10){lines(1:155,ll[i,],col="grey",lwd=2)}
lines(1:155,apply(ll,2,mean),col="red",lwd=2)

#More bagging in caret
predictors = data.frame(ozone=ozone$ozone)
temperature = ozone$temperature
treebag <- bag(predictors, temperature, B = 10,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))


#Example of custom bagging (continued)
plot(ozone$ozone,temperature,col='lightgrey',pch=19)
points(ozone$ozone,predict(treebag$fits[[1]]$fit,predictors),pch=19,col="red")
points(ozone$ozone,predict(treebag,predictors),pch=19,col="blue")


#Parts of bagging
ctreeBag$fit
function (x, y, ...) 
{
        library(party)
        data <- as.data.frame(x)
        data$y <- y
        ctree(y ~ ., data = data)
}



#Parts of bagging
ctreeBag$pred
function (object, x) 
{
        obsLevels <- levels(object@data@get("response")[, 1])
        if (!is.null(obsLevels)) {
                rawProbs <- treeresponse(object, x)
                probMatrix <- matrix(unlist(rawProbs), ncol = length(obsLevels), 
                                     byrow = TRUE)
                out <- data.frame(probMatrix)
                colnames(out) <- obsLevels
                rownames(out) <- NULL
        }
        else out <- unlist(treeresponse(object, x))
        out
}



ctreeBag$aggregate
function (x, type = "class") 
{
        if (is.matrix(x[[1]]) | is.data.frame(x[[1]])) {
                pooled <- x[[1]] & NA
                classes <- colnames(pooled)
                for (i in 1:ncol(pooled)) {
                        tmp <- lapply(x, function(y, col) y[, col], col = i)
                        tmp <- do.call("rbind", tmp)
                        pooled[, i] <- apply(tmp, 2, median)
                }
                if (type == "class") {
                        out <- factor(classes[apply(pooled, 1, which.max)], 
                                      levels = classes)
                }
                else out <- as.data.frame(pooled)
        }
        else {
                x <- matrix(unlist(x), ncol = length(x))
                out <- apply(x, 1, median)
        }
        out
}