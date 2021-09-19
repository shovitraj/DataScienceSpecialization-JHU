#ggplot

library(ggplot2)
str(mpg)
qplot(displ, hwy, data = mpg)
qplot(displ, hwy, data = mpg, color=drv)

qplot(displ, hwy, data = mpg, geom=c("point","smooth"))

qplot(hwy, data=mpg, fill=drv)

qplot(displ, hwy, data=mpg, facets=.~drv) #by column

qplot(hwy, data=mpg, facets=drv~., binwidth=2) #by row

install.packages("maacs")
str(maacs)

download.file("https://github.com/TarekDib03/ExploratoryDataAnalysisCoursera/blob/master/maacs.Rda", "maacs.rda")
maacs <- load("")

download.file("https://github.com/jtleek/modules/blob/master/04_ExploratoryAnalysis/PlottingLattice/maacs_env.rds", "maacs.rds")
maacs<- readRDS("maacs.rds")

download.file("https://github.com/jtleek/modules/blob/master/04_ExploratoryAnalysis/PlottingLattice/maacs_env.rds", "macs_env.rds")
env <- readRDS("maacs_env.rds")
id <- 1:750
maacs <- data.frame(id, env)
save(maacs, file = "maacs.rda")
#qplot(log(eno, data=maacs))
#qplot(log(eno, data=maacs, fill=mopos))
#densityplot
#qplot(log(eno), data = maacs, geom = "density")
#qplot(log(eno), data = maacs, geom = "density", color = mopos)

#scatter plot
#qplot(log(pm25), log(eno), data = maacs)
#qplot(log(pm25), log(eno), data = maacs, shape = mopos)
#qplot(log(pm25), log(eno), data = maacs, color = mopos)

#qplot(log(pm25), log(eno), data = maacs, color = mopos, 
 #     geom = c("point", "smooth"), method = "lm")

#qplot(log(pm25), log(eno), data = maacs, facets = . ~ mopos) + geom_smooth(method = "lm")


