context("stat_apply_group")

library(ggplot2)

test_that("stat_apply_group", {
  my.df.unsorted <- data.frame(X = rep(1:20,2),
                      Y = runif(40)*10,
                      category = rep(c("A","B"), each = 20))

  my.df <- my.df[order(my.df[["X"]]), ]

  result <- ggplot(my.df, aes(x = X, y = Y, colour = category)) +
    geom_point() +
    stat_apply_group(geom = "rug", .fun.y = quantile, .fun.x = quantile)
  result <- ggplot2::layer_data(result,2)
  result <- result[result$group == 1,]$x %>% as.data.frame()
  expected <- quantile(my.df[my.df$category == "A",]$X)[1:5] %>%
    as.data.frame()
  rownames(result) <- NULL
  rownames(expected) <- NULL
  expect_identical(result, expected)

  result <- ggplot(my.df, aes(x = X, y = Y, colour = category)) +
    geom_point() +
    stat_apply_group(geom = "rug", .fun.y = quantile, .fun.x = quantile)
  result <- ggplot2::layer_data(result,2)
  result <- result[result$group == 1,]$y %>% as.data.frame()
  expected <- quantile(my.df[my.df$category == "A",]$Y)[1:5] %>%
    as.data.frame()
  rownames(result) <- NULL
  rownames(expected) <- NULL
  expect_identical(result, expected)

  result <- ggplot(my.df, aes(x = X, y = Y)) +
    geom_point() +
    stat_apply_group(geom = "rug", sides = "lr", color = "darkred",
                     .fun.y = quantile) +
    stat_apply_group(geom = "text", hjust = "right", color = "darkred",
                     .fun.y = quantile,
                     .fun.x = function(x) {rep(22, 5)}, # set x to 22
                     mapping = aes(label = after_stat(y.names))) +
                     expand_limits(x = 21)
  expect_s3_class(result, "ggplot")

  my.probs <- c(0.25, 0.5, 0.75)
  result <- ggplot(my.df, aes(x = X, y = Y, colour = category)) +
    geom_point() +
    stat_apply_group(geom = "hline",
                     aes(yintercept = after_stat(y)),
                     .fun.y = quantile,
                     .fun.y.args = list(probs = my.probs))
  result <- ggplot2::layer_data(result,2)
  result <- result[result$group == 1,]$y %>% as.data.frame()
  expected <- quantile(my.df[my.df$category == "A",]$Y, probs = my.probs) %>%
    as.data.frame()
  rownames(result) <- NULL
  rownames(expected) <- NULL
  expect_identical(result, expected)

  # cummulative summaries
  result <- ggplot(my.df.unsorted, aes(x = X, y = Y, colour = category)) +
    stat_apply_group(.fun.x = function(x) {x},
                     .fun.y = cummax)
  result <- ggplot2::layer_data(result)
  result <- result[result$group == 1,]$y
  expected <- cummax(my.df.unsorted[my.df.unsorted$category == "A",]$Y)
  expect_identical(result, expected)
#
#   ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#     stat_apply_group(.fun.x = cumsum, .fun.y = cumsum)
#
#   # diff returns a shorter vector by 1 for each group
#   ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#     stat_apply_group(.fun.x = function(x) {x[-1L]},
#                      .fun.y = diff, na.rm = TRUE)
#
#   # Running summaries
#   ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#     geom_point() +
#     stat_apply_group(.fun.x = function(x) {x},
#                      .fun.y = runmed, .fun.y.args = list(k = 5))
#
#   # Rescaling per group
#   ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#     stat_apply_group(.fun.x = function(x) {x},
#                      .fun.y = function(x) {(x - min(x)) / (max(x) - min(x))})
})
