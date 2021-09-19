corr <- function(directory, threshold=0) {
  
  listoffiles<- list.files(path=directory, pattern=".csv", full.names=TRUE)
  corr <- numeric()
  
    for(i in 1:332){
      data<- read.csv(listoffiles[i])
      if (sum(complete.cases(data)) > threshold) {
        corr <- c(corr, cor(data[["sulfate"]], data[["nitrate"]], use="complete.obs"))
      }
    }
    corr
}  
  
  
