library(ggpp)

my.cars <- datasets::mtcars[c(TRUE, FALSE, FALSE, FALSE), ]
my.cars$name <- rownames(my.cars)

ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_text(aes(size = wt), nudge_x = -0.1, hjust = "right",
            colour = rgb(0.5,0,0, alpha = 0.5)) +
  scale_radius(range = c(3,6)) + # override scale_area()
  expand_limits(x = c(1.8, 5.5))

### text missing from guide

ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_text_s(aes(size = wt), nudge_x = -0.1, hjust = "right",
              default.colour = rgb(0.5,0,0, 0.5),
              alpha = 0.51,
              colour.target = "text",
              alpha.target = "text") +
  scale_radius(range = c(3,6)) + # override scale_area()
  expand_limits(x = c(1.8, 5.5))

ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_text_s(aes(size = wt), nudge_x = -0.1, hjust = "right") +
  #  scale_radius(range = c(3,6)) + # override scale_area()
  expand_limits(x = c(1.8, 5.5))

ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_text_s(aes(alpha = wt), nudge_x = -0.1, hjust = "right") +
  #  scale_radius(range = c(3,6)) + # override scale_area()
  expand_limits(x = c(1.8, 5.5))

ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_label_s(aes(size = wt), nudge_x = -0.1, hjust = "right",
               fill = "yellow",
               colour = "red",
               alpha = .2,
               default.colour = rgb(0.5,0,0),
               colour = rgb(0,0,0.5, alpha = 0.2),
               colour.target = "text",
               alpha.target = "box.line") +
  scale_radius(range = c(3,6)) + # override scale_area()
  expand_limits(x = c(1.8, 5.5))


ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_label_s(aes(size = wt), nudge_x = -0.1, hjust = "right",
               fill = "yellow",
               colour = "red",
               alpha = .2,
#               default.colour = rgb(0.5,0,0),
               colour.target = c("text", "box.line"),
               alpha.target = c("box.line", "box.fill", "text")) +
  scale_radius(range = c(3,6)) + # override scale_area()
  expand_limits(x = c(1.8, 5.5))

ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_label_s(aes(size = wt), nudge_x = -0.1, hjust = "right",
               fill = "yellow",
               colour = "red",
               alpha = .5,
               #               default.colour = rgb(0.5,0,0),
               colour.target = "none",
               alpha.target = "all") +
  scale_radius(range = c(3,6)) + # override scale_area()
  expand_limits(x = c(1.8, 5.5))
