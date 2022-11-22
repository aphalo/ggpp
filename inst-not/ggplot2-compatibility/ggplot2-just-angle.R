library(ggplot2)

my.cars <- mtcars[c(TRUE, FALSE, FALSE, FALSE), ]

p <- ggplot(my.cars, aes(wt, mpg, label = rownames(my.cars))) +
  geom_point(colour = "red")

p + geom_text(hjust = "outward")

p + geom_text(hjust = "outward", angle = 30)

p + geom_text(hjust = "outward", angle = 70)

p + geom_text(hjust = "outward", angle = 90)

p + geom_text(hjust = "outward", angle = 150)

p + geom_text(hjust = "outward", angle = 220)

p + geom_text(hjust = "outward", angle = 320)

p + geom_text(hjust = "outward", angle = -30)

p + geom_text(hjust = "outward", angle = -70)

p + geom_text(hjust = "outward", angle = -90)

p + geom_text(hjust = "outward", angle = -150)

p + geom_text(hjust = "outward", angle = -220)

p + geom_text(hjust = "outward", angle = -320)

# Issue #4169

print(ggplot(data.frame(x = c(0, 1, 2))) +
        geom_text(aes(
          x = x,
          y = 1,
          label = paste0("Annotation", x),
          angle = x * 60 - 90,
          hjust = "outward"
        )) +
        coord_polar() +
        scale_x_continuous(limits = c(0, 3))
)

