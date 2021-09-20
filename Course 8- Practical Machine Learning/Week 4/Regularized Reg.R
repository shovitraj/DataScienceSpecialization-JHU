#Prostate cancer
library(ElemStatLearn); data(prostate)

instr(prostate)


#Another issue for high-dimensional data
small = prostate[1:5,]

lm(lpsa ~ .,data =small)

