library(ggplot2)
library(ggpmisc)


# x is continuous ---------------------------------------------------------

model <- y ~ x

p01 <- ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  stat_smooth(method = "lm", formula = model)

p01 +
  stat_fit_tb(label.x = 10, label.y = 110) +
  theme_bw()

p01 +
  stat_fit_tb(label.x = 10, label.y = 110, digits = 4) +
  theme_bw()

p01 +
  stat_fit_tb(label.x = 10, label.y = 110, tb.type = "fit.summary") +
  theme_bw()

p01 +
  stat_fit_tb(label.x = 10, label.y = 110, tb.type = "fit.anova") +
  theme_bw()

p01 +
  stat_fit_tb(label.x = 10, label.y = 110, tb.type = "fit.coefs") +
  theme_bw()

# x is factor -------------------------------------------------------------

x <- rep(c("A", "B"), 10)
y <- rnorm(20) + c(0, 0.3)
my.data <- data.frame(x = x, y = y)

p02 <- ggplot(my.data, aes(x, y)) +
  geom_point()

p02
