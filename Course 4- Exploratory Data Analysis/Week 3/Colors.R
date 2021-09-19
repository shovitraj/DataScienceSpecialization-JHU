pal <- colorRamp(c("red", "blue"))
pal(0)
pal(1)
pal(0.5)
pal(seq(0, 1, len = 10))
pal <- colorRampPalette(c("red", "yellow"))
pal(2)
pal(10)

library(RColorBrewer)

cols <- brewer.pal(3, "BuGn")

cols
pal <- colorRampPalette(cols)

image(volcano, col = pal(20))