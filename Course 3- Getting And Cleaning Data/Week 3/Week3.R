#Subsetting and Sorting
library(data.table)
set.seed(13435)
X <- data.frame("var1"=sample(1:5), "var2"=sample(6:10), "var3"=sample(11:15))
X <- X[sample(1:5),]; X$var2[c(1,3)]=NA
X
X[,1]
X[,"var1"]
X[1:2, "var2"]


X[(X$var1 <= 3 & X$var3 >11),]

X[(X$var1 <= 3 | X$var3 >15),]

X[which(X$var2 > 8),]

X

sort(X$var1)
sort(X$var1, decreasing=TRUE)
sort(X$var2, na.last=TRUE)
X[order(X$var1),]
X[order(X$var1, X$var3),]
library(plyr)
arrange(X,var1)
arrange(X, desc(var1))

X$var4 <- rnorm(5)

X




Y <- cbind(X, rnorm(5))


Y



#Summarizing Data


if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile="./data/restaurants.csv", method = "curl")
restData <- read.csv("./data/restaurants.csv")

head(restData, n=3) #Bydefault top 6 rows, n=3 gives 3 row
tail(restData, n=3)
summary(restData)
str(restData)
quantile(restData$councilDistrict,na.rm=TRUE)
quantile(restData$councilDistrict,probs=c(0.5, 0.75, 0.9))
table(restData$zipCode, useNA="ifany")
table(restData$councilDistrict, restData$zipCode)

sum(is.na(restData$councilDistrict))
any(is.na(restData$councilDistrict))
all(restData$zipCode >0)

colSums(is.na(restData))
all(colSums(is.na(restData))==0)

table(restData$zipCode %in% c("21212"))
table(restData$zipCode %in% c("21212", "21213"))

restData[restData$zipCode %in% c("21212", "21213"),]


data("UCBAdmissions")
DF= as.data.frame(UCBAdmissions)
summary(DF)

xt <- xtabs(Freq~Gender + Admit, data=DF)
xt

warpbreaks$replicate <- rep(1:9, len=54)
xt = xtabs(breaks~., data= warpbreaks)
xt
ftable(xt)

#creating new variables

if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile="./data/restaurants.csv", method = "curl")
restData <- read.csv("./data/restaurants.csv")

s1 <- seq(1,10, by=2) ; s1 #starts at one, and increases by 2

s2 <- seq(1,10, length=3) ; s2 #starts at one and ends at 10 with 3 values
x<- c(1,3,8,25,100); seq(along =x) #creates and index to loop over those 5 values


restData$nearMe = restData$neighborhood %in% c("Roland Park", "Homeland")
table(restData$nearMe)


restData$zipWrong = ifelse(restData$zipCode <0, TRUE, FALSE)
table(restData$zipWrong, restData$zipCode < 0)

restData$zipGroups = cut(restData$zipCode,breaks=quantile(restData$zipCode))
table(restData$zipGroups)



table(restData$zipGroups,restData$zipCode)



#Easier Cutting

library(Hmisc)
restData$zipGroups = cut2(restData$zipCode,g=4)
table(restData$zipGroups)

#Creating Factor Variables

restData$zcf <- factor(restData$zipCode)
restData$zcf[1:10]

#Levels of factor vatiables


yesno <- sample(c("yes","no"),size=10,replace=TRUE)
yesnofac = factor(yesno,levels=c("yes","no"))
relevel(yesnofac,ref="no")
as.numeric(yesnofac)

#Cutting produces factor variables

library(Hmisc)
restData$zipGroups = cut2(restData$zipCode,g=4)
table(restData$zipGroups)


#Using the mutate function

library(Hmisc); library(plyr)
restData2 = mutate(restData,zipGroups=cut2(zipCode,g=4))
table(restData2$zipGroups)

#Reshaping Data

library(reshape2)
head(mtcars)

#Melting data frames
mtcars$carname <- rownames(mtcars)
carMelt <- melt(mtcars,id=c("carname","gear","cyl"),measure.vars=c("mpg","hp"))
head(carMelt,n=3)

tail(carMelt, n=3)

#Casting data frames
cylData <- dcast(carMelt, cyl ~ variable)
cylData
cylData <- dcast(carMelt, cyl ~ variable, mean)
cylData

#Averaging Values

head(InsectSprays)
tapply(InsectSprays$count,InsectSprays$spray,sum)

#Another way- split

spIns =  split(InsectSprays$count,InsectSprays$spray)
spIns


#Another Way- combine
sprCount = lapply(spIns, sum)
unlist(sprCount)
sapply(spIns,sum)

#Another Way-plyr package

ddply(InsectSprays,.(spray),summarize,sum=sum(count))

#Creating a new variable
spraySums <- ddply(InsectSprays,.(spray),summarize,sum=ave(count,FUN=sum))
dim(spraySums)
head(spraySums)


#Merging Data
#Peer review data
if(!dir.exists("./data")){dir.create("./data")}
fileUrl1 = "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 = "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1,destfile="./data/reviews.csv",method="curl")
download.file(fileUrl2,destfile="./data/solutions.csv",method="curl")
reviews = read.csv("./data/reviews.csv"); solutions <- read.csv("./data/solutions.csv")
head(reviews,2)
head(solutions,2)

#Merging data - merge()
names(reviews)
names(solutions)

mergedData = merge(reviews,solutions,by.x="solution_id",by.y="id",all=TRUE)
head(mergedData)

#Default-merge all common column names
intersect(names(solutions),names(reviews))
mergedData2 = merge(reviews,solutions,all=TRUE)
head(mergedData2)

#Using join in the plyr package
library(dplyr)
df1 = data.frame(id=sample(1:10),x=rnorm(10))
df2 = data.frame(id=sample(1:10),y=rnorm(10))
arrange(join(df1,df2),id)

df1 = data.frame(id=sample(1:10),x=rnorm(10))
df2 = data.frame(id=sample(1:10),y=rnorm(10))
df3 = data.frame(id=sample(1:10),z=rnorm(10))
dfList = list(df1,df2,df3)
join_all(dfList)
















