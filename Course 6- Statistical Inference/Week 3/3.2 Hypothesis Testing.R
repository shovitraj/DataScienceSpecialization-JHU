#T-test Father Son Data

library(UsingR); data(father.son)
t.test(father.son$sheight - father.son$fheight)