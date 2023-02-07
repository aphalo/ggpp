context("stat_centroid")

set.seed(123456)
my.df <- data.frame(X = rep(1:5,2),
                    Y = runif(10),
                    category = rep(c("A","B"), each = 5))

# make sure rows are ordered for X as we will use functions that rely on this
my.df <- my.df[order(my.df[["X"]]), ]


test_that("stat_centroid, using default shape and size", {
  p1 <- ggplot(my.df, aes(x = X, y = Y, colour = category)) +
    stat_centroid() +
    geom_point()

  result <- layer_data(p1)[, c("xmin", "xmax", "ymin", "ymax", "shape", "size")]
  expected <- data.frame(
    xmin = c(2.292893, 2.292893),
    xmax = c(3.707107, 3.707107),
    ymin = c(0.4278716, 0.2311237),
    ymax = c(0.6303108, 0.5629344),
    shape = c(19, 19),
    size = c(1.5, 1.5)
  )
  expect_identical(format(result, digits = 7), format(expected, digits = 7))
})

test_that("stat_centroid, changed default shape and size", {
  p1 <- ggplot(my.df, aes(x = X, y = Y, colour = category)) +
    stat_centroid(shape = "cross", size = 6) +
    geom_point()

  result <- layer_data(p1)[, c("xmin", "xmax", "ymin", "ymax", "shape", "size")]

  expected <- data.frame(
    xmin = c(2.292893, 2.292893),
    xmax = c(3.707107, 3.707107),
    ymin = c(0.4278716, 0.2311237),
    ymax = c(0.6303108, 0.5629344),
    shape = c("cross", "cross"),
    size = c(6, 6)
  )
  expect_identical(format(result, digits = 7), format(expected, digits = 7))
})

test_that("stat_centroid, geom 'rug' function median applied", {
  p1 <- ggplot(my.df, aes(x = X, y = Y, colour = category)) +
    stat_centroid(geom = "rug", linewidth = 1.5, .fun = median) +
    geom_point()

  result <- layer_data(p1)[, c("x", "y", "linewidth", "linetype")]

  expected <- data.frame(
    x = c(3,3),
    y = c(0.3912557, 0.1983447),
    linewidth = c(1.5, 1.5),
    linetype = c(1, 1)
  )
  expect_identical(format(result, digits = 7), format(expected, digits = 7))
})

