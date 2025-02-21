# example using future 'ggpp' (>= 0.5.9)
# available at https://aphalo.r-universe.dev/ggpp

library(dplyr)

set.seed(123456)
data <- data.frame(group = rep(c("A", "B", "C"), 3)) %>%
  mutate(y = as.numeric(factor(group))*rnorm(n = 9, mean = 0, sd = 0.5),
         x = c(1,1,1,2,2,2,3,3,3))

data_labels <-data %>%
  filter(x == 1)

library(ggplot2)
library(ggrepel)
library(ggpp)

data %>%
  ggplot(aes(x = x, y = y, col = group))+
  geom_line(position = position_dodge(width = .3))+
  geom_text_repel(data= data_labels,
                  aes(label = group),
                  position = position_dodgenudge_to(width = .3, x = .8),
                  direction = "y",
                  hjust = "right",
                  size = 3,
                  segment.size = .5,
                  segment.alpha = .5,
                  segment.linetype = "11",
                  box.padding = .4,
                  segment.curvature = 0.4,
                  segment.ncp = 3,
                  segment.angle = 20,
  ) +
  theme_minimal()
