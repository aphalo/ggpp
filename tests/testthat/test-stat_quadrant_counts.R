context("stat_quadrant_counts")

library(ggplot2)
library(tibble)

make_data_tbl <- function(nrow = 100, rfun = rnorm, ...) {
  if (nrow %% 2) {
    nrow <- nrow + 1
  }

  set.seed(1001)

  tibble::tibble(
    x = rfun(nrow, ...),
    y = rfun(nrow, ...),
    group = rep(c("A", "B"), c(nrow / 2, nrow / 2))
  )
}

test_that("stat_quadrant_counts", {
  p <- ggplot(make_data_tbl(6), aes(x, y)) +
    geom_point() +
    stat_quadrant_counts()
  result <- layer_data(p, i = 2)[, c("npcx", "npcy", "label")] # 2nd layer returns the quadrant statistics.
  expected <- data.frame(
    npcx = c(0.95, 0.05, 0.05, 0.95),
    npcy = c(0.95, 0.05, 0.95, 0.05),
    label = c("n=1", "n=3", "n=2", "n=0")
  )
  expect_identical(result, expected)
})
