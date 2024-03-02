library(ggpmisc)

p0 <-
  ggplot(lynx) +
  geom_line() +
  stat_peaks(colour = "red", colour.target = "all",
             alpha = 0, alpha.target = "point",
             geom = "point_s",
             add.segments = TRUE,
             position = position_nudge_keep(x = 0, y = 400),
             arrow = arrow(length = grid::unit(1.5, "mm")),
             segment.linewidth = 0.75) +
  expand_limits(y = 7400)

p0

p1 <-
  ggplot(lynx) +
  geom_line() +
  geom_point_s(colour = "red", colour.target = "all",
               #             alpha = 0.5, alpha.target = "all",
               position = position_nudge_keep(x = 0, y = 400),
               arrow = arrow(length = grid::unit(1.5, "mm"))) +
  expand_limits(y = 7400)

p1

p2 <-
  ggplot(lynx) +
  geom_line() +
  geom_point_s(colour = "red", colour.target = "all",
               alpha = 0.5, alpha.target = "all",
               position = position_nudge_keep(x = 0, y = 400),
               arrow = arrow(length = grid::unit(1.5, "mm"))) +
  expand_limits(y = 7400)

p1

bench::mark(ggplotGrob(p1),
            ggplotGrob(p2),
            check = FALSE,
            min_iterations = 10)

####################################################

library(ggpp)

### BUG in geom_text_s and geom_label_x

my.cars <- datasets::mtcars[c(TRUE, FALSE, FALSE, FALSE), ]
my.cars$name <- rownames(my.cars)

p1 <-
  ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_text(aes(size = wt), nudge_x = -0.1, hjust = "right") +
  scale_radius(range = c(3,6)) + # override scale_area()
  expand_limits(x = c(1.8, 5.5))

p1

get_guide_data(p1, aesthetic = "size")
get_guide_data(p1, aesthetic = "colour")
get_guide_data(p1, aesthetic = "alpha")

### text missing from guide

p2 <-
  ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_text_s(aes(size = wt), nudge_x = -0.1, hjust = "right") +
  scale_radius(range = c(3,6)) + # override scale_area()
  expand_limits(x = c(1.8, 5.5))

p2

get_guide_data(p2, aesthetic = "size")
get_guide_data(p2, aesthetic = "colour")
get_guide_data(p2, aesthetic = "alpha")

all.equal(p1$guides, p2$guides)

all.equal(p1$labels, p2$labels)

p3 <-
  ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_text_s(aes(size = wt), nudge_x = -0.1, hjust = "right") +
  #  scale_radius(range = c(3,6)) + # override scale_area()
  expand_limits(x = c(1.8, 5.5))

get_guide_data(p3, aesthetic = "size")

p3

###

p4 <-
  ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_label_s(aes(size = wt), nudge_x = -0.1, hjust = "right") +
  scale_radius(range = c(3,6)) + # override scale_area()
  expand_limits(x = c(1.8, 5.5))

get_guide_data(p4, aesthetic = "size")

p4

###

p5 <-
  ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point(color = "red") +
  geom_point_s(aes(size = wt), nudge_x = -0.5) +
  scale_radius(range = c(1,2)) + # override scale_area()
  expand_limits(x = c(1.8, 5.5))

get_guide_data(p5, aesthetic = "size")

p5


#####

library(ggpp)
library(ggrepel)

### BUG in geom_text_s and geom_label_x

my.cars <- datasets::mtcars[c(TRUE, FALSE, FALSE, FALSE), ]
my.cars$name <- rownames(my.cars)

p1 <-
  ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_text(aes(size = wt), nudge_x = -0.1, hjust = "right") +
  scale_radius(range = c(3,6)) + # override scale_area()
  expand_limits(x = c(1.8, 5.5))

get_guide_data(p1, aesthetic = "size")
get_guide_data(p1, aesthetic = "colour")
get_guide_data(p1, aesthetic = "alpha")

p1

### text missing from guide

p2 <-
  ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_text_repel(aes(size = wt), nudge_x = -0.1, hjust = "right") +
  scale_radius(range = c(3,6)) + # override scale_area()
  expand_limits(x = c(1.8, 5.5))

get_guide_data(p2, aesthetic = "size")
get_guide_data(p2, aesthetic = "colour")
get_guide_data(p2, aesthetic = "alpha")

p2

