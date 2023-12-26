library(ggpp)
library(ggbreak)

set.seed(2019-01-19)
d <- data.frame(x = 1:20,
                y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22)
)

p0 <- ggplot(d, aes(y, x)) +
#  geom_col(orientation="y")
  geom_point()

p1 <- p0 +
  geom_text_s(aes(y, x, label=label),
              data = function(z) {cbind(z, label = letters[1:nrow(z)])},
              nudge_x = 0.5,
              box.padding = 0.3,
              colour = 'firebrick')

p2 <- p0 + scale_x_break(c(7, 17)) +
  geom_text_s(aes(y, x, label=label),
              data = function(z) {cbind(z, label = letters[1:nrow(z)])},
              nudge_x = 0.5,
              box.padding = 0.5, # increased value
              colour = 'firebrick')  +
  xlab(NULL) + ylab(NULL) + theme_minimal()

p1 + p2

p3 <- p0 + scale_x_break(c(5, 6)) +
  geom_text_s(aes(y, x, label=label),
              data = function(z) {cbind(z, label = letters[1:nrow(z)])},
              nudge_x = 0.5,
              box.padding = 0.3,
              colour = 'firebrick')  +
  xlab(NULL) + ylab(NULL) + theme_minimal()

p1 + p3

# with zoom in/zoom out the displacement by nudge remains fixed in data units
p4 <- p0 + scale_x_break(c(7, 17), scales = 1/3) +
  geom_text_s(aes(y, x, label=label),
              data = function(z) {cbind(z, label = letters[1:nrow(z)])},
              nudge_x = 0.5,
              box.padding = 0.3,
              colour = 'firebrick')  +
  xlab(NULL) + ylab(NULL) + theme_minimal()

p1 + p4

# with zoom in/zoom out the displacement by nudge remains fixed in data units
# a vector of nudges CANNOT be used (yet)
p5 <- p0 + scale_x_break(c(7, 17), scales = 1/3) +
  geom_text_s(aes(y, x, label=label),
              data = function(z) {cbind(z, label = letters[1:nrow(z)])},
              nudge_x = ifelse(d$y < 7, 0.12, 0.7),
              box.padding = 0.3,
              colour = 'firebrick')  +
  xlab(NULL) + ylab(NULL) + theme_minimal()


p1 + p5

# with zoom in/zoom out the displacement by nudge remains fixed in data units
# a vector of nudges can be used with 'ggplot2'!
p6 <- p0 +
  geom_text(aes(y, x, label=label),
              data = function(z) {cbind(z, label = letters[1:nrow(z)])},
              nudge_x = ifelse(d$y < 7, 0.5, 2),
              colour = 'firebrick')  +
  xlab(NULL) + ylab(NULL) + theme_minimal()

p1 + p6

# with zoom in/zoom out the displacement by nudge remains fixed in data units
# a vector of nudges can be used with position_nudge() from 'ggplot2'!
p7 <- p0 + scale_x_break(c(7, 17), scales = 1/3) +
  geom_text_s(aes(y, x, label=label),
              data = function(z) {cbind(z, label = letters[1:nrow(z)])},
              position = position_nudge(x = ifelse(d$y < 7, 0.2, 1)),
              box.padding = 0.1,
              colour = 'firebrick')  +
  xlab(NULL) + ylab(NULL) + theme_minimal()

p1 + p7

# with zoom in/zoom out the displacement by nudge remains fixed in data units
# a vector of nudges can be used with position_nudge_to() (yet)!
p8 <- p0 + scale_x_break(c(7, 17), scales = 1/3) +
  geom_text_s(aes(y, x, label=label),
              data = function(z) {cbind(z, label = letters[1:nrow(z)])},
              position = position_nudge_to(x = d$y + ifelse(d$y < 7, 0.12, 0.7)),
              box.padding = 0.3,
              colour = 'firebrick') +
  xlab(NULL) + ylab(NULL) + theme_minimal()

p1 + p8

# a vector of nudges can be used with position_nudge_to()
p9 <- p0 +
  geom_text_s(aes(y, x, label=label),
              data = function(z) {cbind(z, label = letters[1:nrow(z)])},
              position = position_nudge_to(x = d$y + 0.7),
              box.padding = 0.3,
              colour = 'firebrick') +
  xlab(NULL) + ylab(NULL) + theme_minimal()

p1 + p9

x_pos <-ifelse(d$y < 7, 0.5/3, 0.5)
p10 <- p0 +
  geom_text_s(aes(y, x, label=label),
              data = function(z) {cbind(z, label = letters[1:nrow(z)])},
              position = position_nudge_keep(x = -0.5),
              box.padding = 0.3,
              colour = 'firebrick') +
  xlab(NULL) + ylab(NULL) + theme_minimal()

p10


