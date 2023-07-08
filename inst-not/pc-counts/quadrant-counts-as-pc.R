library(ggpmisc)  # version > 0.5.3

set.seed(4321)
x <- 1:125
y <- rnorm(length(x), mean = 10)
my.data <- data.frame(x, y)

ggplot(my.data, aes(x, y)) +
  geom_quadrant_lines(colour = "blue", xintercept = 50, yintercept = 10) +
  stat_quadrant_counts(colour = "blue", xintercept = 50, yintercept = 10,
                       mapping = aes(label = after_stat(pc.label)),
                       label.x = c(0.02, 0.95)) +
  geom_point() +
  scale_y_continuous(expand = expansion(mult = 0.15, add = 0))
