context("stat_functions")

expected_sin <- data.frame(
  x = c(0.0000000, 0.3490659, 0.6981317, 1.0471976, 1.3962634, 1.7453293,
        2.0943951, 2.4434610, 2.7925268, 3.1415927),
  idx = 1L,
  xmin = 0,
  xmax = pi,
  y = c(0.000000e+00, 3.420201e-01, 6.427876e-01, 8.660254e-01, 9.848078e-01,
        9.848078e-01, 8.660254e-01, 6.427876e-01, 3.420201e-01, 1.224606e-16)
)
expected_cos <- data.frame(
  x = c(0.0000000, 0.3490659, 0.6981317, 1.0471976, 1.3962634, 1.7453293,
        2.0943951, 2.4434610, 2.7925268, 3.1415927),
  idx = 1L,
  xmin = 0,
  xmax = pi,
  y = c(1.0000000,  0.9396926,  0.7660444,  0.5000000,  0.1736482, -0.1736482,
        -0.5000000, -0.7660444, -0.9396926, -1.0000000)
)

df1 <- data.frame(min = 0, max = pi, fun = I(list(sin)))

test_that("stat_functions, using one function once", {
  p1 <- ggplot(df1, aes(xmin = min, xmax = max, y = fun)) +
    stat_functions(n = 10)

  result <- layer_data(p1)[, c("x", "idx", "xmin", "xmax", "y")]
  expected <- expected_sin

  expect_identical(format(result, digits = 7), format(expected, digits = 7))
})

df2 <- data.frame(min = 0, max = pi,
                  fun = I(list(sin, cos)), name = c("sin", "cos"))

test_that("stat_functions, using two functions", {
  p1 <- ggplot(df2, aes(xmin = min, xmax = max, y = fun, group = after_stat(idx))) +
    stat_functions(n = 10)

  result <- layer_data(p1)[, c("x", "idx", "xmin", "xmax", "y")]

  expected_cos$idx <- 2L
  expected <- rbind(expected_sin, expected_cos)
  expect_identical(format(result, digits = 7), format(expected, digits = 7))
})

