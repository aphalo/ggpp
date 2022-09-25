## ---- include=FALSE, echo=FALSE-----------------------------------------------
library(knitr)
opts_chunk$set(fig.align = 'center', 
               fig.show = 'hold', fig.width = 7, fig.height = 4)
options(warnPartialMatchArgs = FALSE,
        tibble.print.max = 4,
        tibble.print.min = 4,
        dplyr.summarise.inform = FALSE)
eval_all <- FALSE  # if TRUE evaluate all code chunks

## ---- message=FALSE-----------------------------------------------------------
library(ggpp)
library(tibble)
library(dplyr)
eval_ggrepel <- requireNamespace("ggrepel", quietly = TRUE) &&
  packageVersion("ggrepel") > "0.9.1"
if (eval_ggrepel) library(ggrepel)
eval_magick <- requireNamespace("magick", quietly = TRUE)

## -----------------------------------------------------------------------------
old_theme <- theme_set(theme_bw())

## ---- message=FALSE-----------------------------------------------------------
library(ggpp)
theme_set(theme_bw() + theme(panel.grid = element_blank()))

## -----------------------------------------------------------------------------
p1 <- ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_point() +
  geom_text_npc(data = data.frame(x = c("left", "left"),
                                  y = c("top", "bottom"),
                                  label = c("Most\nefficient",
                                            "Least\nefficient")),
                mapping = aes(npcx = x, npcy = y, label = label),
                size = 3)
p1


## -----------------------------------------------------------------------------
p1 + expand_limits(y = 0)

## -----------------------------------------------------------------------------
ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_point() +
  annotate(geom = "text_npc",
           npcx = c("left", "left"),
           npcy = c("top", "bottom"),
           label = c("Most\nefficient",
                     "Least\nefficient"),
           size = 3)

## -----------------------------------------------------------------------------
p2 <- ggplot(mtcars, aes(factor(cyl), mpg, colour = factor(cyl))) +
  stat_boxplot() +
  labs(y = NULL) +
  theme_bw(9) + 
  theme(legend.position = "none",
        panel.grid = element_blank())

ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
  geom_point() +
  annotate("plot_npc", npcx = "left", npcy = "bottom", label = p2) +
  expand_limits(y = 0, x = 0)


## -----------------------------------------------------------------------------
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  geom_text_npc(data = data.frame(cyl = levels(factor(mtcars$cyl)),
                                  label = LETTERS[seq_along(levels(factor(mtcars$cyl)))],
                                  x = 0.90,
                                  y = 0.95),
                mapping = aes(npcx = x, npcy = y, label = label),
                size = 4) +
  facet_wrap(~factor(cyl), scales = "free") +
  theme(strip.background = element_blank(),
        strip.text = element_blank())

## -----------------------------------------------------------------------------
class(lynx)
ggplot(lynx) + geom_line()

## -----------------------------------------------------------------------------
ggplot(lynx, as.numeric = FALSE) + geom_line()

## -----------------------------------------------------------------------------
tb <- mpg %>%
  group_by(cyl) %>%
  summarise(hwy = median(hwy), cty = median(cty))

data.tb <- tibble(x = 7, y = 44, tb = list(tb))

ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_table(data = data.tb, aes(x, y, label = tb)) +
  geom_point() 

## ---- eval=eval_all-----------------------------------------------------------
#  tb <- mpg %>%
#    group_by(cyl) %>%
#    summarise(hwy = median(hwy), cty = median(cty))
#  
#  ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
#    annotate("table", x = 7, y = 44, label = tb) +
#    geom_point()

## -----------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_table(data = data.tb, aes(x, y, label = tb),
             table.theme = ttheme_gtsimple,
             table.hjust = 0, colour = "darkred", fill = "#FFFFBB") +
  geom_point() 

## -----------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_table(data = data.tb, aes(x, y, label = tb),
             table.theme = ttheme_gtlight,
             size = 3, colour = "darkblue",
             stat = "fmt_tb", 
             tb.vars = c(Cylinders = "cyl", MPG = "hwy"), # rename
             tb.rows = 4:1) + # change order
  labs(x = "Engine displacement (l)", y = "Fuel use efficiency (MPG)",
       colour = "Engine cylinders\n(number)") +
  geom_point() +
  theme_bw()

## -----------------------------------------------------------------------------
tb.pm <- tibble(Parameter = c("frac(beta[1], a^2)", "frac(beta[2], a^3)"),
                Value = c("10^2.4", "10^3.532"))
data.tb <- tibble(x = 7, y = 44, tb = list(tb.pm))
ggplot(mpg, aes(displ, cty)) +
  geom_point() +
  geom_table(data = data.tb, aes(x, y, label = tb), parse = TRUE) +
  theme_bw()

## -----------------------------------------------------------------------------
p <- ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_point() 

data.tb <- 
  tibble(x = 7, y = 44, 
         plot = list(p + 
                       coord_cartesian(xlim = c(4.9, 6.2), 
                                       ylim = c(13, 21)) +
                       labs(x = NULL, y = NULL) +
                       theme_bw(8) +
                       scale_colour_discrete(guide = "none")))

ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_plot(data = data.tb, aes(x, y, label = plot)) +
  annotate(geom = "rect", 
           xmin = 4.9, xmax = 6.2, ymin = 13, ymax = 21,
           linetype = "dotted", fill = NA, colour = "black") +
  geom_point() 

## -----------------------------------------------------------------------------
p <- ggplot(mpg, aes(factor(cyl), hwy, fill = factor(cyl))) +
  stat_summary(geom = "col", fun = mean, width = 2/3) +
  labs(x = "Number of cylinders", y = NULL, title = "Means") +
  scale_fill_discrete(guide = "none")

data.tb <- tibble(x = 7, y = 44, 
                  plot = list(p +
                                theme_bw(8)))

ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_plot(data = data.tb, aes(x, y, label = plot)) +
  geom_point() +
  labs(x = "Engine displacement (l)", y = "Fuel use efficiency (MPG)",
       colour = "Engine cylinders\n(number)") +
  theme_bw()

## ---- eval=eval_all-----------------------------------------------------------
#  p <- ggplot(mpg, aes(factor(cyl), hwy, fill = factor(cyl))) +
#    stat_summary(geom = "col", fun = mean, width = 2/3) +
#    labs(x = "Number of cylinders", y = NULL, title = "Means") +
#    scale_fill_discrete(guide = "none")
#  
#  ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
#    annotate("plot", x = 7, y = 44, label = p + theme_bw(8)) +
#    geom_point() +
#    labs(x = "Engine displacement (l)", y = "Fuel use efficiency (MPG)",
#         colour = "Engine cylinders\n(number)") +
#    theme_bw()

## ---- eval=eval_magick--------------------------------------------------------
file.name <- 
  system.file("extdata", "Isoquercitin.png", 
              package = "ggpp", mustWork = TRUE)
Isoquercitin <- magick::image_read(file.name)
grobs.tb <- tibble(x = c(0, 10, 20, 40), y = c(4, 5, 6, 9),
                   width = c(0.05, 0.05, 0.01, 1),
                   height =  c(0.05, 0.05, 0.01, 0.3),
                   grob = list(grid::circleGrob(), 
                               grid::rectGrob(), 
                               grid::textGrob("I am a Grob"),
                               grid::rasterGrob(image = Isoquercitin)))

ggplot() +
  geom_grob(data = grobs.tb, 
            aes(x, y, label = grob, vp.width = width, vp.height = height),
            hjust = 0.7, vjust = 0.55) +
  scale_y_continuous(expand = expansion(mult = 0.3, add = 0)) +
  scale_x_continuous(expand = expansion(mult = 0.2, add = 0)) +
  theme_bw(12)

## ---- eval=(eval_all && eval_magick)------------------------------------------
#  ggplot() +
#    annotate("grob", x = 1, y = 3, vp.width = 0.5,
#             label = grid::rasterGrob(image = Isoquercitin, width = 1)) +
#    theme_bw(12)

## -----------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_vhlines(xintercept = c(2.75, 4), yintercept = 27, linetype = "dashed") +
  geom_point() +
  labs(x = "Engine displacement (l)", y = "Fuel use efficiency (MPG)",
       colour = "Engine cylinders\n(number)") 


## -----------------------------------------------------------------------------
my.cars <- mtcars[c(TRUE, FALSE, FALSE, FALSE), ]
my.cars$name <- rownames(my.cars)
my.cars <- my.cars[order(my.cars$wt), ]
ggplot(my.cars, aes(wt, mpg, label = name)) +
  geom_point() +
  geom_text_s(aes(colour = factor(cyl)),
                   angle = 90,
                   hjust = 1.04, nudge_y = -1, 
                   nudge_x = c(0, 0, -0.05, +0.05, 0, 0 ,0, 0),
                   arrow = arrow(length = grid::unit(1.5, "mm"))) +
  scale_colour_discrete(l = 40) +
  expand_limits(y = 0)

## ---- eval=eval_magick--------------------------------------------------------
file.name <- 
  system.file("extdata", "Robinin.png", 
              package = "ggpp", mustWork = TRUE)
Robinin <- magick::image_read(file.name)

set.seed(123456)
data.tb <- tibble(x = 1:20, y = (1:20) + rnorm(20, 0, 10))

flavo.tb <- tibble(x = 0.02,
                   y = 0.95,
                   width = 1/2,
                   height = 1/4,
                   grob = list(grid::rasterGrob(image = Robinin)))

ggplot(data.tb, aes(x, y)) +
  geom_grob_npc(data = flavo.tb, 
                aes(label = grob, npcx = x, npcy = y, 
                    vp.width = width, vp.height = height)) +
  geom_point() +
  expand_limits(y = 55, x = 0)

## ---- eval=(eval_all && eval_magick)------------------------------------------
#  ggplot(data.tb, aes(x, y)) +
#    geom_grob_npc(label = list(grid::rasterGrob(image = Robinin, width = 1)),
#                  npcx = 0.02, npcy = 0.95,
#                  vp.width = 1/2, vp.height = 1/4) +
#    geom_point() +
#    expand_limits(y = 55, x = 0)

## ---- eval=(eval_all && eval_magick)------------------------------------------
#  ggplot(data.tb, aes(x, y)) +
#    annotate("grob_npc", label = grid::rasterGrob(image = Robinin, width = 1),
#                  npcx = 0.02, npcy = 0.95, vp.width = 1/2, vp.height = 1/4) +
#    geom_point() +
#    expand_limits(y = 55, x = 0)
#  

## -----------------------------------------------------------------------------
corner_letters.tb <- tibble(label = LETTERS[1:4],
                            x = "left", 
                            y = "top",
                            cyl = c(4,5,6,8))
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~cyl, scales = "free") +
  geom_text_npc(data = corner_letters.tb,
                aes(npcx = x, npcy = y, label = label)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

## -----------------------------------------------------------------------------
data.tb <- mpg %>%
  group_by(cyl) %>%
  summarise(hwy = median(hwy), displ = median(displ))
ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_x_margin_point(data = data.tb,
                      aes(xintercept = displ, fill = factor(cyl))) +
  expand_limits(y = 10) +
  geom_point() 

## -----------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_point(alpha = 0.33) +
  stat_centroid(shape = "cross", size = 4)

## ---- eval = FALSE------------------------------------------------------------
#  ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
#    geom_point(alpha = 0.33) +
#    stat_centroid(shape = "cross", size = 4, .fun = median)

## -----------------------------------------------------------------------------
set.seed(4321)
# generate artificial data
x <- -99:100
y <- x + rnorm(length(x), mean = 0, sd = abs(x))
my.data <- data.frame(x, 
                      y, 
                      group = c("A", "B"))

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_quadrant_lines(colour = "red") +
  stat_quadrant_counts(colour = "red") +
  geom_point() +
  expand_limits(y = c(-250, 250))

## ---- eval=eval_all-----------------------------------------------------------
#  ggplot(my.data, aes(x, y)) +
#    geom_quadrant_lines(colour = "red", pool.along = "x") +
#    stat_quadrant_counts(colour = "red", pool.along = "x") +
#    geom_point()

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_quadrant_counts(quadrants = 0L, label.x = "left", 
                       aes(label = sprintf("%i observations", stat(count))))

## ---- eval=eval_all-----------------------------------------------------------
#  ggplot(my.data, aes(x, y)) +
#    geom_quadrant_lines(colour = "red") +
#    stat_quadrant_counts(colour = "red", quadrants = c(2, 4)) +
#    geom_point()

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group)) +
  geom_quadrant_lines() +
  stat_quadrant_counts(geom = "label_npc") +
  geom_point() +
  expand_limits(y = c(-260, 260)) +
  facet_wrap(~group)

## -----------------------------------------------------------------------------
ggplot(Orange, aes(age, circumference, colour = Tree)) +
  stat_apply_group(.fun.x = function(x) {x[-1L]},
                   .fun.y = diff)

## -----------------------------------------------------------------------------
random_string <- function(len = 3) {
paste(sample(letters, len, replace = TRUE), collapse = "")
}

# Make random data.
set.seed(1001)
d <- tibble::tibble(
  x = rnorm(100),
  y = rnorm(100),
  group = rep(c("A", "B"), c(50, 50)),
  lab = replicate(100, { random_string() })
)

## -----------------------------------------------------------------------------
ggplot(data = d, aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(keep.fraction = 1/4, colour = "red")

## ---- eval=eval_all-----------------------------------------------------------
#  ggplot(data = d, aes(x, y)) +
#    geom_point() +
#    stat_dens2d_filter(keep.fraction = 1/4, keep.number = 50, colour = "red")

## ---- eval=eval_all-----------------------------------------------------------
#  ggplot(data = d, aes(x, y)) +
#    geom_point() +
#    stat_dens2d_filter(keep.fraction = 1, keep.number = 50, colour = "red")

## ---- eval=eval_all-----------------------------------------------------------
#  ggplot(data = d, aes(x, y, colour = group)) +
#     stat_dens2d_filter(keep.fraction = 0.25,
#                        size = 3,
#                        colour = "black") +
#     geom_point()

## -----------------------------------------------------------------------------
ggplot(data = d, aes(x + rep(c(-2,2), rep(50,2)), 
                     y, colour = group)) +
   geom_point() +
   stat_dens2d_filter(shape = 1, size = 3,
                      keep.fraction = 0.25)

## -----------------------------------------------------------------------------
ggplot(data = d, aes(x + rep(c(-2,2), rep(50,2)), 
                     y, colour = group)) +
   geom_point() +
   stat_dens2d_filter_g(shape = 1, size = 3,
                      keep.fraction = 0.25)

## ---- eval=eval_all-----------------------------------------------------------
#  ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#    stat_dens2d_labels(keep.fraction = 1/10,
#                       hjust = "outward", vjust = "outward") +
#    geom_point()

## ---- eval=eval_ggrepel-------------------------------------------------------
#  ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#    geom_point() +
#    stat_dens2d_labels(geom = "text_repel",
#                       keep.fraction = 0.45)

## ---- eval=(eval_all && eval_ggrepel)-----------------------------------------
#  ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#    stat_dens2d_labels(geom = "label_repel",
#                       keep.fraction = 0.2,
#                       label.fill = NA) +
#      geom_point()

## ---- eval = FALSE------------------------------------------------------------
#  random_string <- function(len = 6) {
#  paste(sample(letters, len, replace = TRUE), collapse = "")
#  }
#  
#  # Make random data.
#  set.seed(1001)
#  d <- tibble::tibble(
#    x = rnorm(100),
#    y = rnorm(100),
#    group = rep(c("A", "B"), c(50, 50)),
#    lab = replicate(100, { random_string() })
#  )

## -----------------------------------------------------------------------------
ggplot(data = d, aes(x, y)) +
  geom_point() +
  stat_dens1d_filter(keep.fraction = 0.25,
                     colour = "red")

## ---- eval=eval_all-----------------------------------------------------------
#  ggplot(data = d, aes(x, y)) +
#    geom_point() +
#    stat_dens1d_filter(keep.fraction = 0.25,
#                       colour = "red",
#                       orientation = "y")

## ---- eval=eval_ggrepel-------------------------------------------------------
#  ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#    geom_point() +
#    stat_dens1d_labels(geom = "text_repel")

## ---- eval=(eval_all && eval_ggrepel)-----------------------------------------
#  ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#    geom_point() +
#    stat_dens1d_labels(geom = "text_repel", orientation = "y")

## -----------------------------------------------------------------------------
set.seed(84532)
df <- data.frame(
  x = rnorm(20),
  y = rnorm(20, 2, 2),
  l = paste("label:", letters[1:20])
)


## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text_s(position = position_nudge_keep(x = 0.1),
              hjust = "left") +
  expand_limits(x = 2.5)

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text_s(position = position_nudge_keep(x = 0.1),
              hjust = "left", add.segments = FALSE) +
  expand_limits(x = 2.5)

## ---- eval=eval_ggrepel-------------------------------------------------------
#  ggplot(df, aes(x, y, label = l)) +
#    geom_point() +
#    geom_text_repel(position = position_nudge(x = 0.3),
#                    min.segment.length = 0,
#                    max.iter = 0)

## ---- eval=eval_ggrepel-------------------------------------------------------
#  ggplot(df, aes(x, y, label = l)) +
#    geom_point() +
#    geom_text_repel(position = position_nudge_keep(x = 0.3),
#                    min.segment.length = 0, max.iter = 0)

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text(position = position_nudge(x = 0.3))

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = ifelse(x < 1, "", l) )) +
  geom_point() +
  geom_text_s(position = position_nudge_to(y = 2.3),
              segment.color = "red",
              arrow = arrow(length = unit(0.015, "npc")),
              angle = 90,
              hjust = "left") +
  expand_limits(x = 3)

## ---- eval=eval_ggrepel-------------------------------------------------------
#  ggplot(df, aes(x, y, label = l)) +
#    geom_point(color = "red", size = 3) +
#    geom_text_repel(position = position_nudge_to(y = -2.5),
#                    size = 3,
#                    color = "red",
#                    angle = 90,
#                    hjust = 0,
#                    box.padding = 0.1,
#                    segment.color = NA,
#                    direction = "x") +
#    geom_rug(sides = "b", length = unit(0.02, "npc"), color = "red")

## ---- eval=eval_ggrepel-------------------------------------------------------
#  ggplot(df, aes(x, y, label = l)) +
#    geom_point() +
#    geom_text_repel(position =
#                      position_nudge_center(x = 0.3, center_x = 0),
#                      min.segment.length = 0, max.iter = 0)

## ---- eval=eval_ggrepel-------------------------------------------------------
#  ggplot(df, aes(x, y, label = l)) +
#    geom_point() +
#    geom_text_repel(position =
#                      position_nudge_center(x = 0.3,
#                                            direction = "split"),
#                    min.segment.length = 0)

## ---- eval=eval_ggrepel-------------------------------------------------------
#  ggplot(df, aes(x, y, label = l)) +
#    geom_point() +
#    geom_text_repel(position =
#                      position_nudge_center(x = 0.3,
#                                            center_x = 1,
#                                            direction = "split"),
#                    min.segment.length = 0)

## ---- eval=eval_ggrepel-------------------------------------------------------
#  ggplot(df, aes(x, y, label = l)) +
#    geom_point() +
#    geom_text_repel(position =
#                      position_nudge_center(x = 0.3,
#                                            center_x = median,
#                                            direction = "split"),
#                    min.segment.length = 0)

## ---- eval=eval_ggrepel-------------------------------------------------------
#  ggplot(df, aes(x, y, label = l)) +
#    geom_point() +
#    geom_text_repel(position =
#                      position_nudge_center(x = 0.3,
#                                            center_x = function(x) {
#                                              quantile(x,
#                                                       probs = 1/4,
#                                                       names = FALSE)
#                                            },
#                                            direction = "split"),
#                    min.segment.length = 0)

## ---- eval=eval_ggrepel-------------------------------------------------------
#  ggplot(df, aes(x, y, label = l)) +
#    geom_point() +
#    geom_text_repel(angle = 90,
#                    position =
#                      position_nudge_center(y = 0.1,
#                                            direction = "split"))

## ---- eval=eval_ggrepel-------------------------------------------------------
#  ggplot(df, aes(x, y, label = l)) +
#    geom_point() +
#    geom_text_repel(position =
#                      position_nudge_center(x = 0.1,
#                                            y = 0.15,
#                                            direction = "split"))

## ---- eval=eval_ggrepel-------------------------------------------------------
#  ggplot(df, aes(x, y, label = l)) +
#    geom_point() +
#    geom_text_repel(position =
#                      position_nudge_center(x = 0.25,
#                                            y = 0.4,
#                                            direction = "radial"),
#                    min.segment.length = 0)

## ---- eval=eval_ggrepel-------------------------------------------------------
#  ggplot(df, aes(x, y, label = l)) +
#    geom_point() +
#    geom_text_repel(position =
#                      position_nudge_center(x = 0.15,
#                                            y = 0.3,
#                                            center_x = 0,
#                                            center_y = 0,
#                                            direction = "radial"),
#                    min.segment.length = 0,
#                    hjust = "outward", vjust = "outward")

## ---- eval=eval_ggrepel-------------------------------------------------------
#  ggplot(df, aes(x, y, label = l)) +
#    geom_point() +
#    geom_text_repel(position =
#                      position_nudge_center(x = 0.2,
#                                            center_x = 0,
#                                            direction = "split"),
#                    aes(hjust = ifelse(x < 0, 1, 0)),
#                    direction = "y",
#                    min.segment.length = 0) +
#    expand_limits(x = c(-3, 3))

## -----------------------------------------------------------------------------
set.seed(16532)
df <- data.frame(
  x = -10:10,
  y = (-10:10)^2,
  yy = (-10:10)^2 + rnorm(21, 0, 4),
  yyy = (-10:10) + rnorm(21, 0, 4),
  l = letters[1:21]
)

## -----------------------------------------------------------------------------
ggplot(df, aes(x, 2 * x, label = l)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 2, linetype = "dotted") +
  geom_text(position = position_nudge_line(x = -0.5, y = -0.8))

## -----------------------------------------------------------------------------
ggplot(subset(df, x >= 0), aes(x, yyy)) +
  geom_point() +
  stat_smooth(method = "lm") +
  geom_text(aes(label = l),
            vjust = "center", hjust = "center",
            position = position_nudge_line(x = 0, y = 1.2,
                                           method = "lm",
                                           direction = "split"))

## -----------------------------------------------------------------------------
ggplot(subset(df, x >= 0), aes(y, yy)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  geom_text(aes(label = l),
            position = position_nudge_line(method = "lm",
                                           x = 3, y = 3, 
                                           line_nudge = 2.5,
                                           direction = "split"))

## -----------------------------------------------------------------------------
ggplot(subset(df, x >= 0), aes(y, yy)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  geom_text(aes(label = l),
            position = position_nudge_line(abline = c(0, 1),
                                           x = 3, y = 3, 
                                           direction = "split"))

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_line(linetype = "dotted") +
  geom_text(position = position_nudge_line(x = 0.6, y = 6))

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_line(linetype = "dotted") +
  geom_text(position = position_nudge_line(x = -0.6, y = -6))

## -----------------------------------------------------------------------------
ggplot(df, aes(x, yy, label = l)) +
  geom_point() +
  stat_smooth() +
  geom_text(aes(y = yy, label = l),
            position = position_nudge_line(x = 0.6, 
                                           y = 6,
                                           direction = "split"))

## ---- eval=eval_ggrepel-------------------------------------------------------
#  ggplot(df, aes(x, yy, label = l)) +
#    geom_point() +
#    stat_smooth() +
#    geom_label_repel(aes(y = yy, label = paste("point", l)),
#              position = position_nudge_line(x = 0.6,
#                                             y = 6,
#                                             direction = "split"),
#              min.segment.length = 0)

## -----------------------------------------------------------------------------
ggplot(df, aes(x, yy, label = l)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE)) +
  geom_text(aes(y = yy, label = l),
            position = position_nudge_line(method = "lm",
                                           x = 0.5, 
                                           y = 5,
                                           formula = y ~ poly(x, 2, raw = TRUE),
                                           direction = "split"))

## ---- eval=eval_ggrepel-------------------------------------------------------
#  ggplot(df, aes(x, yy, label = l)) +
#    geom_point() +
#    stat_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE)) +
#    geom_text_repel(aes(y = yy, label = l),
#                    box.padding = 0.5,
#                    min.segment.length = Inf)

## ---- eval=eval_ggrepel-------------------------------------------------------
#  ggplot(df, aes(x, yy, label = l)) +
#    geom_point() +
#    stat_smooth() +
#    geom_label_repel(aes(y = yy, label = paste("point", l)),
#                    box.padding = 0.5,
#                    min.segment.length = 0)

## -----------------------------------------------------------------------------
df <- data.frame(x = rep(1:10, 2),
                 y = c(1:10, 10:1),
                 group = rep(c("a", "b"), c(10, 10)),
                 l = "+")

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l, color = group)) +
  geom_line(linetype = "dotted") +
  geom_text() +
  geom_text(position = position_nudge_line(x = 0.25, y = 1)) +
  geom_text(position = position_nudge_line(x = -0.25, y = -1)) +
  coord_equal(ratio = 0.5)

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l, color = group, group = group)) +
  geom_line(linetype = "dotted") +
  geom_text() +
  geom_text(color = "red",
            position = position_nudge_line(x = 1, y = 1)) +
  geom_text(color = "blue",
            position = position_nudge_line(x = -1, y = -1)) +
  coord_equal()

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_line(linetype = "dotted") +
  geom_text() +
  geom_text(position = position_nudge_line(x = 1, y = 1),
            color = "red") +
  geom_text(position = position_nudge_line(x = -1, y = -1),
            color = "blue") +
  facet_wrap(~group) +
  coord_equal(ratio = 1)

## -----------------------------------------------------------------------------
make_data_tbl <- function(nrow = 100, rfun = rnorm, ...) {
  if (nrow %% 2) {
    nrow <- nrow + 1
  }
  
  set.seed(1001)
  
  tibble::tibble(
    x = rfun(nrow, ...),
    y = rfun(nrow, ...),
    group = rep(c("A", "B"), c(nrow / 2, nrow / 2))
  )
}

## ---- eval=eval_all-----------------------------------------------------------
#  ggplot(data = make_data_tbl(300), aes(x, y)) +
#    geom_point() +
#    stat_dens2d_filter(colour = "red",
#                       keep.sparse = FALSE,
#                       keep.fraction = 1/3)

## ---- eval=eval_all-----------------------------------------------------------
#  ggplot(data = make_data_tbl(300), aes(x, y)) +
#    geom_point() +
#    stat_dens2d_filter(colour = "red",
#                       keep.sparse = FALSE,
#                       keep.fraction = 1/3)+
#    stat_dens2d_filter(colour = "blue",
#                       keep.fraction = 1/3)

## ---- eval=eval_all-----------------------------------------------------------
#  ggplot(data = make_data_tbl(300, rfun = runif), aes(x, y)) +
#    geom_point() +
#    stat_dens2d_filter(colour = "red", keep.fraction = 1/2)

## ---- eval=eval_all-----------------------------------------------------------
#  ggplot(data = make_data_tbl(300, rfun = rgamma, shape = 2),
#         aes(x, y)) +
#    geom_point() +
#    stat_dens2d_filter(colour = "red", keep.fraction = 1/3)

## -----------------------------------------------------------------------------
class(austres)
austres.df <- try_tibble(austres)
class(austres.df)
head(austres.df, 4)

## -----------------------------------------------------------------------------
austres.df <- try_tibble(austres, as.numeric = TRUE)
head(austres.df, 4)

## ---- eval=eval_all-----------------------------------------------------------
#  class(lynx)
#  lynx.df <- try_tibble(lynx)
#  class(lynx.df)
#  head(lynx.df, 3)

## -----------------------------------------------------------------------------
lynx.df <- try_tibble(lynx, "year")
head(lynx.df, 3)

## ---- eval=eval_all-----------------------------------------------------------
#  lynx_n.df <- try_tibble(lynx, "year", as.numeric = TRUE)
#  lapply(lynx_n.df, "class")
#  head(lynx_n.df, 3)

## ---- eval=eval_all-----------------------------------------------------------
#  try_tibble(1:5)

## ---- eval=eval_all-----------------------------------------------------------
#  try_tibble(letters[1:5])

## ---- eval=eval_all-----------------------------------------------------------
#  try_tibble(factor(letters[1:5]))

## ---- eval=eval_all-----------------------------------------------------------
#  try_tibble(list(x = rep(1,5), y = 1:5))

## ---- eval=eval_all-----------------------------------------------------------
#  try_tibble(data.frame(x = rep(1,5), y = 1:5))

## ---- eval=eval_all-----------------------------------------------------------
#  try_tibble(matrix(1:10, ncol = 2))

