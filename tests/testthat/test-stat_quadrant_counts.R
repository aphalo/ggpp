context("stat_quadrant_counts")

library(ggplot2)

test_that("stat_quadrant_counts", {
  df <- data.frame(
    x = c(2.2, -0.2, -0.2, -2.5, -0.6, -0.1),
    y = c(1.1, -0.6, -0.9, -1.6, 0.3, 1.6),
    group = c("A", "A", "A", "B", "B", "B")
  )
  p <- ggplot(df, aes(x, y)) +
    stat_quadrant_counts()
  result <- layer_data(p)[, c("npcx", "npcy", "label")]
  expected <- data.frame(
    npcx = c(0.95, 0.05, 0.05, 0.95),
    npcy = c(0.95, 0.05, 0.95, 0.05),
    label = c("n=1", "n=3", "n=2", "n=0")
  )
  expect_identical(result, expected)
})
