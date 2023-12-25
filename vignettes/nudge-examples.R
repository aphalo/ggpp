## ----include=FALSE, echo=FALSE------------------------------------------------
library(knitr)
opts_chunk$set(fig.align = 'center', 
               fig.show = 'hold', fig.width = 7, fig.height = 4)
options(warnPartialMatchArgs = FALSE,
        tibble.print.max = 4,
        tibble.print.min = 4,
        dplyr.summarise.inform = FALSE)

## ----message = FALSE----------------------------------------------------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpp)
library(grid)
# Is a compatible version of 'ggrepel' installed?
eval_ggrepel <- requireNamespace("ggrepel", quietly = TRUE) &&
  packageVersion("ggrepel") >= "0.9.2"
if (eval_ggrepel) library(ggrepel)

old_theme <- theme_set(theme_bw())

## -----------------------------------------------------------------------------
ggplot(data.frame(x = 1:10, y = rnorm(10)), aes(x, y)) +
  geom_point() +
  geom_point_s(nudge_x = 0.5, colour = "red")

## -----------------------------------------------------------------------------
set.seed(84532)
df <- data.frame(
  x = rnorm(20),
  y = rnorm(20, 2, 2),
  l = paste("label:", letters[1:20])
)


## ----eval=eval_ggrepel--------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text_repel(position = position_nudge_keep(x = 0.3),
                  max.iter = 0)

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text_s(position = position_nudge_keep(x = 0.1),
              min.segment.length = 0) +
  expand_limits(x = 2.3)

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text_s(position = position_nudge_keep(x = 0.1),
              min.segment.length = 0,
              hjust = "left") +
  expand_limits(x = 2.3)

## ----eval=eval_ggrepel--------------------------------------------------------
## Example data frame where each species' principal components have been computed.
df1 <- data.frame(
  Species = paste("Species",1:5),
  PC1     = c(-4, -3.5, 1, 2, 3),
  PC2     = c(-1, -1, 0, -0.5, 0.7)) 

ggplot(df1, aes(x=PC1, y = PC2, label = Species, colour = Species)) +
  geom_hline(aes(yintercept = 0), linewidth = .2) +
  geom_vline(aes(xintercept = 0), linewidth = .2) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_label_repel(position = position_nudge_center(x = 0.2, y = 0.01,
                                                    center_x = 0, center_y = 0),
                   label.size = NA,
                   label.padding = 0.1,
                   fill = rgb(red = 1, green = 1, blue = 1, alpha = 0.75)) +
  xlim(-5, 5) +
  ylim(-2, 2) +
  # Stadard settings for displaying biplots
  coord_fixed() +
  theme(legend.position = "none")

## -----------------------------------------------------------------------------
ggplot(df1, aes(x=PC1, y = PC2, label = Species, colour = Species)) +
  geom_hline(aes(yintercept = 0), linewidth = .2) +
  geom_vline(aes(xintercept = 0), linewidth = .2) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_label_repel(label.size = NA,
                   label.padding = 0.1,
                   fill = rgb(red = 1, green = 1, blue = 1, alpha = 0.75)) +
  xlim(-5, 5) +
  ylim(-2, 2) +
  # Stadard settings for displaying biplots
  coord_fixed() +
  theme(legend.position = "none")

## -----------------------------------------------------------------------------
ggplot(df1, aes(x=PC1, y = PC2, label = Species, colour = Species)) +
  geom_hline(aes(yintercept = 0), linewidth = .2) +
  geom_vline(aes(xintercept = 0), linewidth = .2) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_label(position = position_nudge_center(x = 0.2, y = 0.01,
                                              center_x = 0, center_y = 0),
             label.size = 0,
             vjust = "outward", hjust = "outward",
             fill = rgb(red = 1, green = 1, blue = 1, alpha = 0.75)) +
  xlim(-5, 5) +
  ylim(-2, 2) +
  # Stadard settings for displaying biplots
  coord_fixed() +
  theme(legend.position = "none")

## ----eval=eval_ggrepel--------------------------------------------------------
ggplot(df, aes(x, y, label = ifelse(x < 0.5, "", l) )) +
  geom_point() +
  geom_text_repel(position = 
                    position_nudge_to(x = 2.3),
                  min.segment.length = 0,
                  segment.color = "red",
                  arrow = arrow(length = unit(0.015, "npc")),
                  direction = "y") +
  expand_limits(x = 3)

## -----------------------------------------------------------------------------
size_from_area <- function(x) {sqrt(max(0, x) / pi)}

df2 <- data.frame(b = exp(seq(2, 4, length.out = 10)))

ggplot(df2, aes(1, b, size = b)) + 
  geom_text_s(aes(label = round(b,2)),
              position = position_nudge_to(x = c(1.1, 0.9)),
              box.padding = 0.5) +
  geom_point() +
  scale_size_area() +
  xlim(0, 2) +
  theme(legend.position = "none")

## ----eval=eval_ggrepel--------------------------------------------------------
keep <- c("Israel", "United States", "European Union", "China", "South Africa", "Qatar",
          "Argentina", "Chile", "Brazil", "Ukraine", "Indonesia", "Bangladesh")

data <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")
data$date <- ymd(data$date)

data %>%
  filter(location %in% keep) %>%
  select(location, date, total_vaccinations_per_hundred) %>%
  arrange(location, date) %>%
  filter(!is.na(total_vaccinations_per_hundred)) %>%
  mutate(location = factor(location),
         location = reorder(location, total_vaccinations_per_hundred)) %>%
  group_by(location) %>% # max(date) depends on the location!
  mutate(label = if_else(date == max(date), 
                         as.character(location), 
                         "")) -> owid

ggplot(owid,
       aes(x = date, 
           y = total_vaccinations_per_hundred,
           color = location)) +
  geom_line() +
  geom_text_repel(aes(label = label),
                  size = 3,
                  position = position_nudge_to(x = max(owid$date) + days(30)),
                  segment.color = 'grey',
                  point.size = 0,
                  box.padding = 0.1,
                  point.padding = 0.1,
                  hjust = "left",
                  direction = "y") + 
  scale_x_date(expand = expansion(mult = c(0.05, 0.2))) +
  labs(title = "Cumulative COVID-19 vaccination doses administered per 100 people",
       y = "",
       x = "Date (year-month)") +
  theme_bw() +
  theme(legend.position = "none")

## ----eval=eval_ggrepel--------------------------------------------------------
ggplot(df, aes(x, y, label = round(x, 2))) +
  geom_point(size = 3) +
  geom_text_repel(position = position_nudge_to(y = -2.7), 
            size = 3,
            angle = 90,
            hjust = 0,
            box.padding = 0.05,
            min.segment.length = Inf,
            direction = "x",
            force = 0.1,
            force_pull = 0.1) +
  geom_rug(sides = "b", length = unit(0.02, "npc"))

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_point_s(position = 
                    position_nudge_center(x = 0.3, center_x = 0),
               colour = "red")

## ----eval=eval_ggrepel--------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_text_repel(position = 
                    position_nudge_center(x = 0.3, center_x = 0),
                  min.segment.length = 0)

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_point() +
  geom_point_s(position = 
                    position_nudge_center(x = 0.3, center_x = 1),
               colour = "red")

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y)) +
  geom_vline(xintercept = median(df$x), linetype = "dashed") +
  geom_point() +
  geom_point_s(position = 
                    position_nudge_center(x = 0.3, center_x = median),
               colour = "red")

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y)) +
  geom_point() +
  geom_point_s(position = 
                    position_nudge_center(x = 0.3, y = 0.3,
                                          center_x = function(x) {
                                            quantile(x, 
                                                     probs = 1/4, 
                                                     names = FALSE)
                                          },
                                          center_y = 2,
                                          direction = "split"),
               colour = "red")

## ----eval=eval_ggrepel--------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text_repel(angle = 90,
                  position = 
                    position_nudge_center(y = 0.1,
                                          direction = "split"))

## ----eval=eval_ggrepel--------------------------------------------------------
ggplot(df, aes(x, y)) +
  stat_centroid(shape = "+", size = 5, colour = "red") +
  geom_point() +
  geom_point_s(position = 
                    position_nudge_center(x = 0.2,
                                          y = 0.3,
                                          direction = "split"),
               colour = "red")

## ----eval=eval_ggrepel--------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text_repel(position = 
                    position_nudge_center(x = 0.1,
                                          y = 0.15,
                                          direction = "split"))

## ----eval=eval_ggrepel--------------------------------------------------------
ggplot(df, aes(x, y)) +
  stat_centroid(shape = "+", size = 5, colour = "red") +
  geom_point() +
  geom_point_s(position = 
                    position_nudge_center(x = 0.25,
                                          y = 0.4,
                                          direction = "radial"),
               colour = "red")

## ----eval=eval_ggrepel--------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text_repel(position = 
                    position_nudge_center(x = 0.25,
                                          y = 0.4,
                                          direction = "radial"),
                  min.segment.length = 0)

## ----eval=eval_ggrepel--------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text_repel(position = 
                    position_nudge_center(x = 0.125,
                                          y = 0.25,
                                          center_x = 0,
                                          center_y = 0,
                                          direction = "radial"),
                  min.segment.length = 0,
                  hjust = "outward", vjust = "outward") +
  expand_limits(x = c(-2.7, +2.3))

## ----eval=eval_ggrepel--------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text_repel(position = 
                    position_nudge_center(x = 0.2,
                                          center_x = 0,
                                          direction = "split"),
                  aes(hjust = "outward"),
                  direction = "y",
                  min.segment.length = 0) +
  expand_limits(x = c(-3, 3))

## ----eval=eval_ggrepel--------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  stat_dens2d_labels(geom = "text_repel",
                     keep.fraction = 1/3,
                     position = 
                       position_nudge_center(x = 0.2,
                                             center_x = 0,
                                             direction = "split"),
                     aes(hjust = ifelse(x < 0, 1, 0)),
                     direction = "y",
                     min.segment.length = 0) +
  stat_dens2d_filter(geom = "point",
                     keep.fraction = 1/3,
                     shape = "circle open", size = 3) +
  expand_limits(x = c(-3, 3))

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

## ----eval=eval_ggrepel--------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, colour = group)) +
  geom_point() +
  stat_dens2d_labels(geom = "text_repel", 
                     keep.fraction = 0.45)

## ----eval=eval_ggrepel--------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, colour = group)) +
  stat_dens2d_labels(geom = "label_repel", 
                     keep.fraction = 0.2, 
                     label.fill = NA) +
    geom_point()

## -----------------------------------------------------------------------------
library(ggplot2)
library(ggpp)
library(ggrepel)
syms = c(letters[1:5], LETTERS[1:5], 0:9)
labs = do.call(paste0, expand.grid(syms, syms))
dset = data.frame(x=rnorm(1e3), y=rnorm(1e3), label=sample(labs, 1e3, replace=TRUE))

## -----------------------------------------------------------------------------
ggplot(dset, aes(x=x, y=y, label = label)) +
  geom_point(colour = "grey85") +
  stat_dens2d_filter(geom = "text_repel",
                     position = position_nudge_centre(x = 0.1, 
                                                      y = 0.1, 
                                                      direction = "radial"),
                     keep.number = 50,
                     keep.these = c("aA", "bB", "cC"),
                     min.segment.length = 0) +
  theme_bw()

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
  geom_point_s(position = position_nudge_line(x = -1, y = -2),
               colour = "red")

## -----------------------------------------------------------------------------
ggplot(subset(df, x >= 0), aes(x, yyy)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  geom_point_s(position = position_nudge_line(x = 0, y = 1.2,
                                              method = "lm",
                                              direction = "split"),
               colour = "red")

## -----------------------------------------------------------------------------
ggplot(subset(df, x >= 0), aes(y, yy)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  geom_point_s(position = position_nudge_line(method = "lm",
                                              x = 1.5, y = 3, 
                                              line_nudge = 2.75,
                                              direction = "split"),
               colour = "red")

## -----------------------------------------------------------------------------
ggplot(subset(df, x >= 0), aes(y, yy)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  geom_point_s(position = position_nudge_line(abline = c(0, 1),
                                              x = 3, y = 6, 
                                              direction = "split"),
               colour = "red")

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y)) +
  geom_point() +
  geom_line(linetype = "dotted") +
  geom_point_s(position = position_nudge_line(x = 0.6, y = 6),
               colour = "red")

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y)) +
  geom_point() +
  geom_line(linetype = "dotted") +
  geom_point_s(position = position_nudge_line(x = -0.6, y = -6),
               colour = "red")

## -----------------------------------------------------------------------------
ggplot(df, aes(x, yy)) +
  geom_point() +
  stat_smooth(method = "loess", formula = y ~ x) +
  geom_point_s(position = position_nudge_line(x = 0.6, y = 6,
                                              direction = "split"),
               colour = "red")

## ----eval=eval_ggrepel--------------------------------------------------------
ggplot(df, aes(x, yy)) +
  geom_point() +
  stat_smooth(method = "loess", formula = y ~ x) +
  geom_label_repel(aes(y = yy, label = paste("point", l)),
                   position = position_nudge_line(x = 0.6, 
                                                  y = 8,
                                                  direction = "split"),
                   box.padding = 0.3,
                   min.segment.length = 0)

## ----eval=eval_ggrepel--------------------------------------------------------
ggplot(df, aes(x, yy)) +
  geom_point() +
  stat_smooth(method = "loess", formula = y ~ x) +
  geom_label_repel(aes(y = yy, label = paste("point", l)),
                  box.padding = 0.5,
                  min.segment.length = 0)

## -----------------------------------------------------------------------------
ggplot(df, aes(x, yy)) +
  geom_point() +
  stat_smooth(method = "loess", formula = y ~ x) +
  geom_label_s(aes(y = yy, label = paste("point", l)),
               position = position_nudge_line(x = 0.6, 
                                              y = 8,
                                              direction = "split"),
               box.padding = 0,
               min.segment.length = 0)

## -----------------------------------------------------------------------------
ggplot(df, aes(x, yy)) +
  geom_point() +
  stat_smooth(method = "lm", 
              formula = y ~ poly(x, 2, raw = TRUE)) +
  geom_text_repel(aes(y = yy, label = l),
                  position = position_nudge_line(method = "lm",
                                                 formula = y ~ poly(x, 2, raw = TRUE),
                                                 x = 0.5, 
                                                 y = 5,
                                                 direction = "split"),
                  box.padding = 0.25,
                  min.segment.length = Inf)

## -----------------------------------------------------------------------------
ggplot(df, aes(x, yy)) +
  geom_point() +
  stat_smooth(method = "lm", 
              formula = y ~ poly(x, 2, raw = TRUE)) +
  geom_text(aes(y = yy, label = l),
            position = position_nudge_line(method = "lm",
                                           formula = y ~ poly(x, 2, raw = TRUE),
                                           x = 0.5, 
                                           y = 5,
                                           direction = "split"))

## ----eval=eval_ggrepel--------------------------------------------------------
ggplot(df, aes(x, yy)) +
  geom_point() +
  stat_smooth(method = "lm", 
              formula = y ~ poly(x, 2, raw = TRUE)) +
  geom_text_repel(aes(y = yy, label = l),
                  box.padding = 0.25,
                  min.segment.length = Inf)

## ----eval=eval_ggrepel--------------------------------------------------------
df <- tibble::tribble(
  ~y, ~x, ~grp,
  "a", 1,  "some long name",
  "a", 2,  "other name",
  "b", 1,  "some name",
  "b", 3,  "another name",
  "b", -1, "some long name"
)

ggplot(data = df, aes(x, y, group = grp)) +
  geom_col(aes(fill = grp), width=0.5) +
  geom_vline(xintercept = 0) +
  geom_label_repel(aes(label = grp),
                   position = position_stacknudge(vjust = 0.5, y = 0.4),
                   label.size = NA)

