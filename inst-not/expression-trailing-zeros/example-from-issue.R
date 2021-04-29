library(tidyverse)
library(ggpmisc)

set.seed(2017)

tdf <- data.frame(
  x = 1:100,
  y = 1.04 * 1:100 + rnorm(100)
)

ggplot(aes(x = x, y = y), data = tdf) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(
    aes(label = paste(stat(eq.label), stat(rr.label), sep = "*plain(\",\")~")),
    formula = y ~ x,
    coef.digits = 2,
    rr.digits = 3,
    parse = TRUE
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = 2)
