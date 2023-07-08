context("stat_quadrant_counts")

library(ggplot2)

test_that("stat_quadrant_counts", {
  tst.df <- data.frame(
    x = c(2.2, -0.2, -0.2, -2.5, -0.6, -0.1),
    y = c(1.1, -0.6, -0.9, -1.6, 0.3, 1.6),
    group = c("A", "A", "A", "B", "B", "B")
  )
  vdiffr::expect_doppelganger("stat_quadrant_counts_1",
                              ggplot(tst.df, aes(x, y)) +
                                stat_quadrant_counts())

  p <- ggplot(tst.df, aes(x, y)) +
    stat_quadrant_counts()
  result <- layer_data(p)[, c("npcx", "npcy", "label", "quadrant", "count",
                              "total", "count.label", "pc.label", "fr.label", "hjust", "vjust")]
  expected <- data.frame(
    npcx = c(0.95, 0.05, 0.05, 0.95),
    npcy = c(0.95, 0.05, 0.95, 0.05),
    label = c("n=1", "n=3", "n=2", "n=0"),
    quadrant = c(1L, 3L, 4L, 2L),
    count = c(1L, 3L, 2L, 0L),
    total = 6L,
    count.label = c("n=1", "n=3", "n=2", "n=0"),
    pc.label = c("p=17%", "p=50%", "p=33%", "p=0%"),
    fr.label = c("f=0.17", "f=0.50", "f=0.33", "f=0.00"),
    hjust = "inward",
    vjust = "inward"
  )
  expect_identical(result, expected)

  p <- ggplot(tst.df, aes(x, y)) +
    stat_quadrant_counts(digits = 3)
  result <- layer_data(p)[, c("npcx", "npcy", "label", "quadrant", "count",
                              "total", "count.label", "pc.label", "fr.label", "hjust", "vjust")]
  expected <- data.frame(
    npcx = c(0.95, 0.05, 0.05, 0.95),
    npcy = c(0.95, 0.05, 0.95, 0.05),
    label = c("n=1", "n=3", "n=2", "n=0"),
    quadrant = c(1L, 3L, 4L, 2L),
    count = c(1L, 3L, 2L, 0L),
    total = 6L,
    count.label = c("n=1", "n=3", "n=2", "n=0"),
    pc.label = c("p=16.7%", "p=50.0%", "p=33.3%", "p=0.0%"),
    fr.label = c("f=0.167", "f=0.500", "f=0.333", "f=0.000"),
    hjust = "inward",
    vjust = "inward"
  )
  expect_identical(result, expected)

  p <- ggplot(tst.df, aes(x, y)) +
    stat_quadrant_counts(quadrant = 0)
  result <- layer_data(p)[, c("npcx", "npcy", "label", "quadrant", "count", "total", "hjust", "vjust")]
  expected <- data.frame(
    npcx = 0.95,
    npcy = 0.95,
    label = "n=6",
    quadrant = 0L,
    count = 6L,
    total = 6L,
    hjust = "inward",
    vjust = "inward")
  expect_equal(result, expected) # not identical!

  p <- ggplot(tst.df, aes(x, y)) +
    stat_quadrant_counts(pool.along = "x")
  result <- layer_data(p)[, c("npcx", "npcy", "label", "quadrant", "count", "total", "hjust", "vjust")]
  expected <- data.frame(
    npcx = 0.5,
    npcy = c(0.95, 0.05),
    label = "n=3",
    quadrant = c(1L, 2L),
    count = 3L,
    total = 6L,
    hjust = "inward",
    vjust = "inward")
  expect_identical(result, expected)

  p <- ggplot(tst.df, aes(x, y)) +
    stat_quadrant_counts(pool.along = "y")
  result <- layer_data(p)[, c("npcx", "npcy", "label", "quadrant", "count", "total", "hjust", "vjust")]
  expected <- data.frame(
    npcx = c(0.95, 0.05),
    npcy = 0.5,
    label = c("n=1", "n=5"),
    quadrant = c(1L, 4L),
    count = c(1L, 5L),
    total = 6L,
    hjust = "inward",
    vjust = "inward")
  expect_identical(result, expected)

  p <- ggplot(tst.df, aes(x, y)) +
    stat_quadrant_counts(geom = "text")
  result <- layer_data(p)[, c("label", "x", "y", "quadrant", "count", "total", "hjust", "vjust")]
  expected <- data.frame(
    label = c("n=1", "n=3", "n=2", "n=0"),
    x = c(2.2, -2.5, -2.5, 2.2),
    y = c(1.6, -1.6, 1.6, -1.6),
    quadrant = c(1L, 3L, 4L, 2L),
    count = c(1L, 3L, 2L, 0L),
    total = 6L,
    hjust = 0.5,
    vjust = 0.5
  )
  expect_identical(result, expected)

  expect_error(ggplot(tst.df, aes(x, y)) +
                 stat_quadrant_counts(label.x = NA))

  expect_error(ggplot(tst.df, aes(x, y)) +
                 stat_quadrant_counts(label.y = NA))

  expect_error(ggplot(tst.df, aes(x, y)) +
                 stat_quadrant_counts(pool.along = "z"))

  expect_error(ggplot(tst.df, aes(x, y)) +
                 stat_quadrant_counts(quadrants = numeric(5)))

})
