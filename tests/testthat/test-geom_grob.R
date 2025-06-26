context("geom_grob")

library(ggplot2)
library(tibble)

test_that("character label gives error in geom_grob", {
  my.df <- data.frame(x = 1:10, y = 1:10, tb = letters[1:10])
  expect_error(print(ggplot(my.df, aes(x, y, label = tb)) +
                       geom_grob()))
})

test_that("numeric label gives error in geom_grob", {
  my.df <- data.frame(x = 1:10, y = 1:10, tb = 1:10)
  expect_error(print(ggplot(my.df, aes(x, y, label = tb)) +
                       geom_grob()))
})

test_that("geom_grob pos_or_nudge", {
  tb <- tibble(a = 2:4, b = 4:2)
  my.tb <- tibble(x = 0, y = 0, tb = list(tb))
  expect_error(geom_grob(data = my.tb,
                          mapping = aes(x, y, label = tb),
                          position = "stack",
                          nudge_x = 0.5,
                          nudge_y = 0.5),
               "You must specify either `position` or `nudge_x`/`nudge_y`.")
})

test_that("geom_grob no segment", {
  tb <- tibble(a = 2:4, b = 4:2, c = 1:3)
  df <- tibble(x = 2, y = 3, grob = list(grid::circleGrob(r = 0.2)))
  p <- ggplot(data = tb, aes(a,b)) +
    geom_point(aes(colour = factor(c))) +
    geom_grob(data = df,
              aes(x, y, label = grob))

  result <- layer_data(p, 2)[, c("x", "y", "label", "colour")]
  expect_identical(result$colour, "black")
  expect_equal("x_orig" %in% colnames(result), FALSE)
  expect_identical(df$x, result$x)
  expect_identical(df$y, result$y)
})

test_that("geom_grob segment drawn", {
  tb <- tibble(a = 2:4, b = 4:2, c = 1:3)
  df <- tibble(x = 2, y = 3, grob = list(grid::circleGrob(r = 0.2)))
  nudge_x <- 0.8
  colour <- "blue"

  p1 <- ggplot(data = tb, aes(a,b)) +
    geom_point(aes(colour = factor(c))) +
    geom_grob(data = df,
              aes(x, y, label = grob),
              nudge_x = nudge_x,
              colour = colour)

  result1 <- layer_data(p1, 2)[, c("x", "y", "label", "x_orig", "y_orig", "colour")]
  expect_identical(df$x + nudge_x, result1$x)
  expect_identical(df$x, result1$x_orig)
  expect_identical(colour, "blue")
})

test_that("geom_grob segment disabled", {
  tb <- tibble(a = 2:4, b = 4:2, c = 1:3)
  df <- tibble(x = 2, y = 3, grob = list(grid::circleGrob(r = 0.2)))
  nudge_x = 0.8

  p2 <- ggplot(data = tb, aes(a,b)) +
    geom_point(aes(colour = factor(c))) +
    geom_grob(data = df,
              aes(x, y, label = grob),
              add.segments = FALSE,
              nudge_x = nudge_x)

  result2 <- layer_data(p2, 2)[, c("x", "y", "label", "x_orig", "y_orig", "colour")]
  expect_identical(result2$colour, "black")
  expect_identical(df$x + nudge_x, result2$x)
  expect_identical(df$y, result2$y_orig)
})

test_that("geom_grob_npc", {
  tb <- tibble(a = 2:4, b = 4:2, c = 1:3)
  df <- tibble(x = 2, y = 3, grob = list(grid::circleGrob(r = 0.2)))

  p_npc <- ggplot(data = tb, aes(a,b)) +
    geom_point(aes(colour = factor(c))) +
    geom_grob_npc(data = df,
                  aes(npcx = x, npcy = y, label = grob))

  result_p_npc <- layer_data(p_npc, 2)[, c("npcx", "npcy", "label", "colour")]
  expect_identical(result_p_npc$colour, "black")
  expect_identical(result_p_npc$npcx, df$x)
  expect_identical(result_p_npc$npcy, df$y)
})
