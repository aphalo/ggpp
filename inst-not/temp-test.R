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
                     keep.these = c("aA", "bB", "cC"),
                     min.segment.length = 0) +
  theme_bw()

ggplot(dset, aes(x=x, y=y, label = label)) +
  geom_point(colour = "grey85") +
  stat_dens2d_filter(geom = "text_repel",
                     position = position_nudge_centre(x = 0.1,
                                                      y = 0.1,
                                                      direction = "radial"),
                     keep.number = 50,
                     keep.these = c(T, F, F, F, F, F, F, F),
                     min.segment.length = 0) +
  theme_bw()

ggplot(dset, aes(x=x, y=y, label = label)) +
  geom_point(colour = "grey85") +
  stat_dens2d_filter(geom = "text_repel",
                     position = position_nudge_centre(x = 0.1,
                                                      y = 0.1,
                                                      direction = "radial"),
                     keep.number = 50,
                     keep.these = 1:10,
                     min.segment.length = 0) +
  theme_bw()

ggplot(dset, aes(x=x, y=y, label = label)) +
  geom_point(colour = "grey85") +
  stat_dens2d_filter(geom = "text_repel",
                     position = position_nudge_centre(x = 0.1,
                                                      y = 0.1,
                                                      direction = "radial"),
                     keep.number = 0,
                     keep.these = 1:10,
                     exclude.these = 1:5,
                     min.segment.length = 0) +
  theme_bw()

ggplot(dset, aes(x=x, y=y, label = label)) +
  geom_point(colour = "grey85") +
  stat_dens2d_filter(geom = "text_repel",
                     position = position_nudge_centre(x = 0.1,
                                                      y = 0.1,
                                                      direction = "radial"),
                     keep.number = 0,
                     keep.these = 1:10,
                     exclude.these = "5D",
                     min.segment.length = 0) +
  theme_bw()

