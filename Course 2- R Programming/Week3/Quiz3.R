library(datasets)
data(iris)

library(data.table)
iris_dt <- as.data.table(iris)
iris_dt[Species == "virginica",(mean(Sepal.Length)) ]
iris_dt[Species == "virginica",round((mean(Sepal.Length)))]
apply(iris[, 1:4], 2, mean)

library(datasets)
data(mtcars)
?mtcars

tapply(mtcars$mpg, mtcars$cyl, mean)

sapply(split(mtcars$mpg, mtcars$cyl), mean)

lapply(mtcars, mean)

apply(mtcars, 2, mean)

sapply(mtcars, cyl, mean)

with(mtcars, tapply(mpg, cyl, mean))

mean(mtcars$mpg, mtcars$cyl)

mtcars_dt <- as.data.table(mtcars)
mtcars_dt <- mtcars_dt[,  .(mean_cols = mean(hp)), by = cyl]
round(abs(mtcars_dt[cyl == 4, mean_cols] - mtcars_dt[cyl == 8, mean_cols]))

debug(ls)
ls(
        
)
