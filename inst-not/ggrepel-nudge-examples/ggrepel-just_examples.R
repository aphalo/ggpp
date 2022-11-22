library(ggplot2)
library(ggrepel)

my.cars <- mtcars[c(TRUE, FALSE, FALSE, FALSE), ]

p <- ggplot(my.cars, aes(wt, mpg, label = rownames(my.cars))) +
  geom_point(colour = "red")

# problem 1 opposite direction for geom_text() and geom_text_repel()
p + geom_text(hjust = "outward")

p + geom_text_repel(hjust = "outward")

p + geom_text(hjust = "inward")

p + geom_text_repel(hjust = "inward")

p + geom_text(hjust = "right")

p + geom_text_repel(hjust = "right")

p + geom_text(hjust = "left")

p + geom_text_repel(hjust = "left")

# Angles > 45 degrees or < -45 degrees
# Same problem as the one now fixed in 'ggplt2'
p + geom_text_repel(hjust = "outward", angle = 30)

p + geom_text_repel(hjust = "inward", angle = 30)

p + geom_text_repel(hjust = "outward", angle = 70)

p + geom_text_repel(hjust = "inward", angle = 70)

p + geom_text_repel(hjust = "outward", angle = 90)

p + geom_text_repel(hjust = "inward", angle = 90)

p + geom_text_repel(hjust = "outward", angle = -90)

p + geom_text_repel(hjust = "inward", angle = -90)
