pollutantmean <- function(directory, pollutant, id=1:332){
     listoffiles<- list.files(path=directory, pattern=".csv", full.names=TRUE)
     data.values <- numeric()
     
     
     for (i in id){
       data<- read.csv(listoffiles[i])
       data.values <- c(data.values, data[[pollutant]]) #"sulfate" or "nitrate"
     }
     mean(data.values, na.rm=TRUE)
}

#pollutantmean("./specdata", "sulfate")
#pollutantmean("./specdata", "nitrate")