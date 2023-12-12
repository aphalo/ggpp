# https://stackoverflow.com/questions/48692705/text-repel-with-a-position-argument-in-ggplot-r?rq=2

library(ggplot2)
library(ggpp)
library(ggrepel)

data("mtcars")
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)

ggplot(mtcars, aes(x = am, y = mpg, label = mpg)) +
  geom_point(
    mapping = aes(color = cyl),
    position = position_dodge(1),
    size = 3,
    alpha = 0.5
  ) +
  geom_text_repel(
    mapping = aes(group = cyl),
    position = position_dodgenudge(width = 1,
                                   y = 0, x = c(0.1)),
    size = 2.5,
    alpha = 0.9,
    min.segment.length = 0,
    direction = "y"
  )

ggplot(mtcars, aes(x = am, y = mpg, label = mpg)) +
  geom_point(
    mapping = aes(color = cyl),
    position = position_dodge(1),
    size = 3,
    alpha = 0.5
  ) +
  geom_text_repel(
    mapping = aes(group = cyl),
    position = position_dodgenudge(width = 1, y = 0, x = c(-0.08, 0.08)),
    size = 4,
    alpha = 0.9,
    min.segment.length = 0,
    direction = "y"
  )
