## ---- include=FALSE, echo=FALSE-----------------------------------------------
library(knitr)
opts_chunk$set(fig.align = 'center', 
               fig.show = 'hold', fig.width = 7, fig.height = 4)
options(warnPartialMatchArgs = FALSE,
        tibble.print.max = 4,
        tibble.print.min = 4,
        dplyr.summarise.inform = FALSE)

## -----------------------------------------------------------------------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpp)
library(ggrepel)

old_theme <- theme_set(theme_bw())

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
  geom_text_repel(position = position_nudge_keep(x = 0.3),
                  min.segment.length = 0, max.iter = 0)

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text_repel(position = position_nudge(x = 0.3),
                  min.segment.length = 0, 
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

## -----------------------------------------------------------------------------
## Example data frame where each species' principal components have been computed.
df1 <- data.frame(
  Species = paste("Species",1:5),
  PC1     = c(-4, -3.5, 1, 2, 3),
  PC2     = c(-1, -1, 0, -0.5, 0.7)) 

ggplot(df1, aes(x=PC1, y = PC2, label = Species, colour = Species)) +
  geom_hline(aes(yintercept = 0), size=.2) +
  geom_vline(aes(xintercept = 0), size=.2) +
  geom_segment(aes(x=0, y=0, xend=PC1, yend=PC2), 
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
              box.padding = 0) +
  geom_point() +
  scale_size_area() +
  xlim(0, 2) +
  theme(legend.position = "none")

## -----------------------------------------------------------------------------
keep <- c("Israel", "United States", "European Union", "China", "South Africa", "Qatar",
          "Argentina", "Chile", "Brazil", "Ukraine", "Indonesia", "Bangladesh")

data <- readr::read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv", lazy = FALSE, show_col_types = FALSE) 

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
                  direction="y") + 
  scale_x_date(expand = expansion(mult = c(0.05, 0.2))) +
  labs(title = "Cumulative COVID-19 vaccination doses administered per 100 people",
       y = "",
       x = "Date (year-month)") +
  theme_bw() +
  theme(legend.position = "none")

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = round(x, 2))) +
  geom_point(color = "red", size = 3) +
  geom_text_repel(position = position_nudge_to(y = -2.7), 
            size = 3,
            color = "red",
            angle = 90,
            hjust = 0,
            box.padding = 0.05,
            min.segment.length = Inf,
            direction = "x",
            force = 0.1,
            force_pull = 0.1) +
  geom_rug(sides = "b", length = unit(0.02, "npc"), color = "red")

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text_repel(position = 
                    position_nudge_center(x = 0.3, center_x = 0),
                    min.segment.length = 0, max.iter = 0)

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text_repel(position = 
                    position_nudge_center(x = 0.3,
                                          direction = "split"),
                  min.segment.length = 0)

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text_repel(position = 
                    position_nudge_center(x = 0.3,
                                          center_x = 1,
                                          direction = "split"),
                  min.segment.length = 0)

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text_repel(position = 
                    position_nudge_center(x = 0.3,
                                          center_x = median,
                                          direction = "split"),
                  min.segment.length = 0)

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text_repel(position = 
                    position_nudge_center(x = 0.3,
                                          center_x = function(x) {
                                            quantile(x, 
                                                     probs = 1/4, 
                                                     names = FALSE)
                                          },
                                          direction = "split"),
                  min.segment.length = 0)

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text_repel(angle = 90,
                  position = 
                    position_nudge_center(y = 0.1,
                                          direction = "split"))

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text_repel(position = 
                    position_nudge_center(x = 0.1,
                                          y = 0.15,
                                          direction = "split"))

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text_repel(position = 
                    position_nudge_center(x = 0.25,
                                          y = 0.4,
                                          direction = "radial"),
                  min.segment.length = 0)

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text_s(position = 
                    position_nudge_center(x = 0.125,
                                          y = 0.2,
                                          center_x = -0.5,
                                          direction = "radial"),
                  min.segment.length = 0) +
  expand_limits(x = c(-2.7, +2.3))

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y, label = l)) +
  geom_point() +
  geom_text_repel(position = 
                    position_nudge_center(x = 0.2,
                                          center_x = 0,
                                          direction = "split"),
                  aes(hjust = ifelse(x < 0, 1, 0)),
                  direction = "y",
                  min.segment.length = 0) +
  expand_limits(x = c(-3, 3))

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
  stat_smooth(method = "lm", formula = y ~ x) +
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
ggplot(df, aes(x, yy)) +
  geom_point() +
  stat_smooth(method = "loess", formula = y ~ x) +
  geom_text(aes(y = yy, label = l),
            position = position_nudge_line(x = 0.6, 
                                           y = 6,
                                           direction = "split"))

## -----------------------------------------------------------------------------
ggplot(df, aes(x, yy)) +
  geom_point() +
  stat_smooth(method = "loess", formula = y ~ x) +
  geom_label_repel(aes(y = yy, label = paste("point", l)),
                   position = position_nudge_line(x = 0.6, 
                                                  y = 8,
                                                  direction = "split"),
                   box.padding = 0.2,
                   min.segment.length = 0)

## -----------------------------------------------------------------------------
ggplot(df, aes(x, yy)) +
  geom_point() +
  stat_smooth(method = "loess", formula = y ~ x) +
  geom_label_repel(aes(y = yy, label = paste("point", l)),
                  box.padding = 0.5,
                  min.segment.length = 0)

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

## -----------------------------------------------------------------------------
ggplot(df, aes(x, yy)) +
  geom_point() +
  stat_smooth(method = "lm", 
              formula = y ~ poly(x, 2, raw = TRUE)) +
  geom_text_repel(aes(y = yy, label = l),
                  box.padding = 0.5,
                  min.segment.length = Inf)

## -----------------------------------------------------------------------------
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

