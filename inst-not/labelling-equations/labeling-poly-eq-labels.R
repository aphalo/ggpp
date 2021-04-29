# https://stackoverflow.com/questions/61357383/label-ggplot-with-group-names-and-their-equation-possibly-with-ggpmisc
# @ Mark Neal

library(tidyverse)
library(ggpmisc)

df_mtcars <- mtcars %>% mutate(factor_cyl = as.factor(cyl))

my_formula <- y ~ 1

p <- ggplot(df_mtcars, aes(x = wt, y = mpg, group = factor_cyl, colour = factor_cyl)) +
  geom_smooth(method="lm", formula = my_formula) +
  geom_point()+
  stat_poly_eq(formula = my_formula,
               label.x = "centre",
               eq.with.lhs = "",
               aes(label = paste("bold(\"", c("4", "6", "8")[stat(group)],
                                 " cylinders:  \")*",
                                 "italic(hat(y))~`=`~",
                                 stat(eq.label),
                                 sep = "")),
               label.x.npc = "right",
               parse = TRUE) +
  scale_colour_discrete(guide = FALSE)
p

p <- ggplot(df_mtcars, aes(x = wt, y = mpg, group = factor_cyl, colour = factor_cyl)) +
  geom_smooth(method = "lm", formula = my_formula) +
  geom_point()+
  stat_poly_eq(formula = my_formula,
                parse = TRUE)
p
