library(ggplot2)
library(gapminder)
library(dplyr)

data <- gapminder

g<- ggplot(data, aes(x = year, y = lifeExp, color=continent)) +
        geom_point(size=1.5) +
        geom_smooth(aes(fill=continent), method="lm")
        xlab("Year") +
        ylab("Life Expectancy")
g

data %>%
        group_by(continent, year) %>%
        summarise(lifeExp=median(lifeExp)) %>%
        ggplot(aes(x=year, y=lifeExp, color=continent)) +
        geom_line(size=1) + 
        geom_point(size=1.5)

data %>%
        group_by(continent, year) %>%
        summarise(lifeExp=median(lifeExp)) %>% head()

gap1 <- ggplot(data=gapminder, aes(x=continent, y=lifeExp, fill=continent))
gap1 +
        geom_boxplot(outlier.size=2)
gap1