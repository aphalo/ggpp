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
                      block = c("a", "a", "b", "b"),
                      wt = sqrt(x))


formula <- y ~ x + I(x^2) + I(x^3)
fig <-
  ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = stat(eq.label)),
               eq.with.lhs = "italic(hat(y))~`=`~",
               formula = formula,
               size = 5,
               parse = TRUE) +
  # stat_fit_tb(method = "lm",
  #             method.args = list(formula = formula),
  #             tb.vars = c(Parameter = "term",
  #                         Estimate = "estimate",
  #                         "s.e." = "std.error",
  #                         "italic(t)" = "statistic",
  #                         "italic(P)" = "p.value"),
  #             label.y.npc = "center", label.x.npc = "left",
  #             vjust = 0L,
  #             size = 4,
  #             parse = TRUE) +
  stat_fit_tb(method = "lm",
              method.args = list(formula = formula),
              tb.type = "fit.anova",
              tb.vars = c(Effect = "term",
                          "df",
                          "M.S." = "meansq",
                          "italic(F)" = "statistic",
                          "italic(P)" = "p.value"),
              label.y.npc = "center", label.x.npc = "left",
              vjust = 0L,
              size = 5,
              parse = TRUE) +
  theme_bw(16)

fig

png("inst-not/ggplot2-exts/ggpmisc.png", width = 700, height = 600)
print(fig)
dev.off()
