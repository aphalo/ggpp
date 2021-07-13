# test ggpp with ggbreak

library(dplyr)
library(ggpp)
library(ggbreak)

tb <- mpg %>%
  group_by(cyl) %>%
  summarise(hwy = median(hwy), cty = median(cty))

data.tb <- tibble(x = 7, y = 44, tb = list(tb))

ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_table(data = data.tb, aes(x, y, label = tb)) +
  geom_point()

ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_table(data = data.tb, aes(x, y, label = tb)) +
  expand_limits(y = c(0, 45)) +
  scale_y_break(c(5, 10)) +
  geom_point()

ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  annotate(geom = "table", x = 7, y = 44, label = tb ) +
  expand_limits(y = c(0, 45)) +
  scale_y_break(c(5, 10)) +
  geom_point()

ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  annotate(geom = "table_npc", npcx = "right", npcy = "top", label = tb ) +
  expand_limits(y = c(0, 45)) +
  scale_y_break(c(5, 10)) +
  geom_point()

ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  annotate(geom = "text", x = 7, y = 44, label = "test" ) +
  expand_limits(y = c(0, 45)) +
  scale_y_break(c(5, 10)) +
  geom_point()

ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  annotate(geom = "text_npc", npcx = "right", npcy = "top", label = "test" ) +
  expand_limits(y = c(0, 45)) +
  scale_y_break(c(5, 10)) +
  geom_point()




ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  annotate(geom = "table", x = 7, y = 44, label = tb ) +
  expand_limits(y = c(0, 45)) +
  scale_y_cut(c(5, 10)) +
  geom_point()

ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  annotate(geom = "table_npc", npcx = "right", npcy = "top", label = tb ) +
  expand_limits(y = c(0, 45)) +
  scale_y_break(c(5, 10)) +
  geom_point()

ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  annotate(geom = "text", x = 7, y = 44, label = "test" ) +
  expand_limits(y = c(0, 45)) +
  scale_y_break(c(5, 10)) +
  geom_point()

ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  annotate(geom = "text_npc", npcx = "right", npcy = "top", label = "test" ) +
  expand_limits(y = c(0, 45)) +
  scale_y_break(c(5, 10)) +
  geom_point()

