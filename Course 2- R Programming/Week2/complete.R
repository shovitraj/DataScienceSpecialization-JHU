complete <- function(directory, id=1:332){
  listoffiles<- list.files(path=directory, pattern=".csv", full.names=TRUE)
  nobs <- numeric()
  
  for (i in id){
    data<- read.csv(listoffiles[i])
    nobs <- c(nobs, sum(complete.cases(data))) #"sulfate" or "nitrate"
  }
  data.frame(id, nobs)
}