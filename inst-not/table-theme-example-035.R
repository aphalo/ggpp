tb <- mpg %>%
  group_by(cyl) %>%
  summarise(hwy = median(hwy), cty = median(cty))
data.tb <- tibble(x = 7, y = 44, tb = list(tb))

p <- ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_table(data = data.tb, aes(x, y, label = tb),
             table.theme = ttheme_gtstripes,
             size = 4,
             stat = "fmt_tb", tb.vars = c(Cylinders = "cyl", "MPG" = "hwy")) +
  labs(x = "Engine displacement (l)", y = "Fuel use efficiency (MPG)",
       colour = "Engine cylinders\n(number)") +
  geom_point(size = 2.5) +
  theme_dark()
p
svg("table-035-example.svg", height = 5, width = 8)
print(p)
dev.off()

p <- ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_table(data = data.tb, aes(x, y, label = tb),
             table.theme = ttheme_gtdark,
             size = 4,
             stat = "fmt_tb", tb.vars = c(Cylinders = "cyl", "MPG" = "hwy")) +
  labs(x = "Engine displacement (l)", y = "Fuel use efficiency (MPG)",
       colour = "Engine cylinders\n(number)") +
  geom_point(size = 2.5) +
  theme_grey()
p
svg("table-035-example2.svg", height = 5, width = 8)
print(p)
dev.off()
