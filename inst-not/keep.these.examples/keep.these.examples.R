library(ggplot2)
library(ggpp)
library(ggrepel)
syms = c(letters[1:5], LETTERS[1:5], 0:9)
labs = do.call(paste0, expand.grid(syms, syms))
dset = data.frame(x=rnorm(1e3), y=rnorm(1e3), label=sample(labs, 1e3, replace=TRUE))
ggplot(dset, aes(x=x, y=y, label = label)) +
  geom_point(colour = "grey85") +
  stat_dens2d_filter(geom = "text_repel",
                     position = position_nudge_centre(x = 0.1, 
                                                      y = 0.1, 
                                                      direction = "radial"),
                     keep.number = 50,
                     keep.these = function(x) {x %in% c("aA", "bB", "cC")},
                     min.segment.length = 0) +
  theme_bw()

ggplot(dset, aes(x=x, y=y, label = label)) +
  geom_point(colour = "grey85") +
  stat_dens2d_filter(geom = "text_repel",
                     position = position_nudge_centre(x = 0.1, 
                                                      y = 0.1, 
                                                      direction = "radial"),
                     keep.number = 50,
                     keep.these = c("aA", "bB", "cC"),
                     min.segment.length = 0) +
  theme_bw()

library(magrittr)

ggplot(dset, aes(x=x, y=y, label = label)) +
  geom_point(colour = "grey85") +
  stat_dens2d_filter(data = . %>% subset(y >= 0),
                     geom = "text_repel",
                     position = position_nudge_centre(x = 0.1, 
                                                      y = 0.1, 
                                                      center_y = 0, 
                                                      direction = "radial"),
                     keep.number = 25,
                     keep.these = c("aA", "bB", "cC"),
                     min.segment.length = 0) +
  theme_bw()
