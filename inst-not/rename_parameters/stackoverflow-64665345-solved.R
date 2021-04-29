## ggpmisc (>= 0.3.7)
library(ggpmisc)

## dummy data
set.seed(1)
df <- data.frame(month = c(1:60))
df$observed <- 2.5 + 0.05*df$month + rnorm(60, sd = 1)

## min plot example
my.formula <- y ~ poly(x,2,raw=TRUE) ## formula with generic variable names

plt <- ggplot(df, aes(x=month, y=observed)) +
  geom_point() +
  ## show fit and CI
  geom_smooth(method = "lm", se=TRUE, level=0.95, formula = my.formula) +
  ## display equation with useful variable names (i.e. not x and y)
  stat_poly_eq(eq.with.lhs = "italic(Obs)~`=`~",
               eq.x.rhs = '" month"',
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE,
               formula = my.formula, label.y = 0.9) +
  ## show table of each coefficient's p-value
  stat_fit_tb(method.args = list(formula = my.formula),
              tb.vars = c(parameter = "term", ## can change column headings
                          coeff = "estimate",
                          "p-val" = "p.value"),
              tb.params = c(1, month = 2, "month^2" = 3), ##
              label.y = 0.8, label.x = "left",
              parse = TRUE)

plt

####
## ggpmisc (>= 0.3.7)
library(ggpmisc)

## dummy data
set.seed(1)
df <- data.frame(month = c(1:60))
df$observed <- 2.5 + 0.05*df$month + rnorm(60, sd = 1)

## min plot example
my.formula <- y ~ poly(x,2,raw=TRUE) ## formula with generic variable names

plt <- ggplot(df, aes(x=month, y=observed)) +
  geom_point() +
  ## show fit and CI
  geom_smooth(method = "lm", se=TRUE, level=0.95, formula = my.formula) +
  ## display equation with useful variable names (i.e. not x and y)
  stat_poly_eq(eq.with.lhs = "italic(Obs)~`=`~",
               eq.x.rhs = '" month"',
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE,
               formula = my.formula, label.y = 0.9) +
  ## show table of each coefficient's p-value
  stat_fit_tb(method.args = list(formula = my.formula),
              tb.vars = c(coeff = "estimate",
                          "p-val" = "p.value"),
              tb.params = c(1, month = 2, "month^2" = 3), ##
              label.y = 0.8, label.x = "left",
              parse = TRUE)

plt

plt <- ggplot(df, aes(x=month, y=observed)) +
  geom_point() +
  ## show fit and CI
  geom_smooth(method = "lm", se=TRUE, level=0.95, formula = my.formula) +
  ## display equation with useful variable names (i.e. not x and y)
  stat_poly_eq(eq.with.lhs = "italic(Obs)~`=`~",
               eq.x.rhs = '" month"',
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE,
               formula = my.formula, label.y = 0.9) +
  ## show table of each coefficient's p-value
  stat_fit_tb(method.args = list(formula = my.formula),
              tb.vars = c(parameter = "term", ## can change column headings
                          coeff = "estimate",
                          "p-val" = "p.value"),
              tb.params = c(month = 2, "month^2" = 3), ##
              label.y = 0.8, label.x = "left",
              parse = TRUE)

plt

### Question
## dummy data
set.seed(1)
df <- data.frame(month = c(1:60))
df$observed <- 2.5 + 0.05*df$month + rnorm(60, sd = 1)

## min plot example
my.formula <- y ~ poly(x,2,raw=TRUE) ## formula with generic variable names

plt <- ggplot(df, aes(x=month, y=observed)) +
  geom_point() +
  ## show fit and CI
  geom_smooth(method = "lm", se=TRUE, level=0.95, formula = my.formula) +
  ## display equation with useful variable names (i.e. not x and y)
  stat_poly_eq(eq.with.lhs = "italic(Obs)~`=`~",
               eq.x.rhs = ".month",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE,
               formula = my.formula, label.y = 0.9) +
  ## show table of each coefficient's p-value
  stat_fit_tb(method.args = list(formula = my.formula),
              tb.vars = c(parameter = "term", ## can change column headings
                          coeff = "estimate",
                          "p-val" = "p.value"),
              label.y = 0.8, label.x = "left")

plt

