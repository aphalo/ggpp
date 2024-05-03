library(tidyverse)
library(ggrepel)
library(ggpp)

country.labels <- c("Chile" = "República de Chile", "United States" = "United States of America")

# Using a repulsive geom from 'ggrepel'
gapminder::gapminder |>
  filter(year == 2002) |>
  ggplot() +
  aes(gdpPercap, lifeExp) +
  geom_point(colour = "darkgrey") +
  geom_text_repel(
    aes(label = ifelse(country %in% names(country.labels),
                       wrap_labels(country.labels, width = 15)[as.character(country)],
                       "")),
    hjust = 0,
    nudge_y = c(-6, 4),
    nudge_x = 1000,
    na.rm = TRUE,
    box.padding = 0.3,
    point.padding = 0.3,
    arrow = arrow(length = unit(2, "mm"), type = "closed")
  )

# Using a non-repulsive geom 'ggpp'
gapminder::gapminder |>
  filter(year == 2002) |>
  ggplot() +
  aes(gdpPercap, lifeExp) +
  geom_point(colour = "darkgrey") +
  geom_text_s(
    aes(label = wrap_labels(country.labels, width = 15)[as.character(country)]),
    hjust = 0.5,
    nudge_y = c(-3, 3),
    nudge_x = c(-1000, 100),
    na.rm = TRUE,
    box.padding = 1,
    point.padding = 1,
    arrow = arrow(length = unit(2, "mm"), type = "closed")
  )
