## Code included in P. J. Aphalo's answers in Stackoverflow
## If any of these stop working, the answers will need editing

library(ggplot2)
library(ggpmisc)
df <- data.frame(x = c(1:100))
df$y <- 2 + 3 * df$x + rnorm(100, sd = 40)
my.formula <- y ~ x
p <- ggplot(data = df, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", formula = my.formula) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., stat(rr.label), sep = "~~~")),
               parse = TRUE) +
  geom_point()
p

library(ggplot2)
library(ggpmisc)
df <- data.frame(x = c(1:100))
df$y <- 2 + 3 * df$x + rnorm(100, sd = 40)
my.formula <- y ~ x
p <- ggplot(data = df, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", formula = my.formula) +
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., stat(rr.label), sep = "~~~")),
               parse = TRUE) +
  geom_point()
p

p <- ggplot(data = df, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", formula = my.formula) +
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(h)~`=`~",
               eq.x.rhs = "~italic(z)",
               aes(label = stat(eq.label)),
               parse = TRUE) +
  labs(x = expression(italic(z)), y = expression(italic(h))) +
  geom_point()
p


library(ggplot2)
library(ggpmisc)

# generate artificial data
set.seed(4321)
x <- 1:100
y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
my.data <- data.frame(x,
                      y,
                      group = c("A", "B"),
                      y2 = y * c(0.5,2),
                      block = c("a", "a", "b", "b"))

str(my.data)

# plot
ggplot(data = my.data, mapping=aes(x = x, y = y2, colour = group)) +
  geom_point() +
  geom_smooth(method = "lm", se =  FALSE, formula = y ~ poly(x=x, degree = 2, raw = TRUE)) +
  stat_poly_eq(
    mapping     = aes(label = paste(..eq.label.., stat(rr.label), sep = "~~~"))
    , formula     = y ~ poly(x, 2, raw = TRUE)
    , eq.with.lhs = "hat(Y)~`=`~"
    , eq.x.rhs    = "X"
    , parse       = TRUE
  ) +
  theme_bw()

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpmisc)

ozone.df <- read_csv("ggpmisc.csv", col_types = "cd")
ozone.df <- mutate(ozone.df, datetime = dmy_hm(date))
ggplot(ozone.df, aes(datetime, o3)) + geom_line() +
  stat_peaks(colour = "red", span = 21, ignore_threshold = 0.5) +
  stat_peaks(geom = "text", colour = "red", span = 21, ignore_threshold = 0.5,
             hjust = -0.1, x.label.fmt = "%H:%M", angle = 90) +
  ylim(0, 85)
