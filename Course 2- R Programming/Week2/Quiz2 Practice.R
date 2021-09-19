data<-read.csv("specdata/001.csv")
complete.cases()
complete.cases(data)
sum(complete.case(data))
filelist<-list.files(path="specdata", pattern=" ")
filelist<-list.files(path="specdata", pattern=".csv")
length(filelist)
filelist
filelist<-list.files(path="specdata", pattern=".csv", full.names = TRUE)
filelist
filelist[1]

read.csv(filelist[1])
complete.cases(read.csv(filelist[1]))
sum(complete.cases(read.csv(filelist[2])))

nobs <- numeric()
nobs <- c(nobs, 117)
nobs

nobs <- c(nobs, sum(complete.cases(read.csv(filelist[2]))))
nobs
nobs <- sum(complete.cases(read.csv(filelist[3])))


RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))