library(ggpmisc)

ggplot(lynx) +
  geom_line() +
  stat_peaks(colour = "red", colour.target = "all",
             alpha = 0, alpha.target = "point",
             geom = "point_s",
             add.segments = TRUE,
             position = position_nudge_keep(x = 0, y = 400),
             arrow = arrow(length = grid::unit(1.5, "mm"), ends = "first"),
             segment.linewidth = 0.75) +
  expand_limits(y = 7400)

p1 <-
  ggplot(lynx) +
  geom_line() +
  geom_point_s(colour = "red", colour.target = "all",
#             alpha = 0.5, alpha.target = "all",
             position = position_nudge_keep(x = 0, y = 400),
             arrow = arrow(length = grid::unit(1.5, "mm"))) +
  expand_limits(y = 7400)

p2 <-
  ggplot(lynx) +
  geom_line() +
  geom_point_s(colour = "red", colour.target = "all",
               alpha = 0.5, alpha.target = "all",
               position = position_nudge_keep(x = 0, y = 400),
               arrow = arrow(length = grid::unit(1.5, "mm"))) +
  expand_limits(y = 7400)

bench::mark(ggplotGrob(p1),
            ggplotGrob(p2),
            check = FALSE,
            min_iterations = 10)


### BUG in geom_text_s and geom_label_x

my.cars <- datasets::mtcars[c(TRUE, FALSE, FALSE, FALSE), ]
my.cars$name <- rownames(my.cars)

ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_text(aes(size = wt), nudge_x = -0.1, hjust = "right") +
  scale_radius(range = c(3,6)) + # override scale_area()
  expand_limits(x = c(1.8, 5.5))

### text missing from guide

ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_text_s(aes(size = wt), nudge_x = -0.1, hjust = "right") +
  scale_radius(range = c(3,6)) + # override scale_area()
  expand_limits(x = c(1.8, 5.5))

ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_text_s(aes(size = wt), nudge_x = -0.1, hjust = "right") +
#  scale_radius(range = c(3,6)) + # override scale_area()
  expand_limits(x = c(1.8, 5.5))

ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_text_s(nudge_x = -0.1, hjust = "right") +
  expand_limits(x = c(1.8, 5.5))

###

ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_label_s(aes(size = wt), nudge_x = -0.1, hjust = "right") +
  scale_radius(range = c(3,6)) + # override scale_area()
  expand_limits(x = c(1.8, 5.5))

ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_label_s(aes(colour = wt), nudge_x = -0.1, hjust = "right") +
  expand_limits(x = c(1.8, 5.5))

ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_label_s(aes(fill = wt), nudge_x = -0.1, hjust = "right",
               colour = "white", colour.target = c("text", "box")) +
  expand_limits(x = c(1.8, 5.5))

###

ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point(color = "red") +
  geom_point_s(aes(size = wt), nudge_x = -0.5) +
  scale_radius(range = c(1,2)) + # override scale_area()
  expand_limits(x = c(1.8, 5.5))

ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_label_s(aes(colour = wt), nudge_x = -0.1, hjust = "right") +
  expand_limits(x = c(1.8, 5.5))

ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_label_s(aes(fill = wt), nudge_x = -0.1, hjust = "right",
               colour = "white", colour.target = c("text", "box")) +
  expand_limits(x = c(1.8, 5.5))

