context("stat_dens2d_filter_g")

random_string <-
  function(len = 6) {
    paste(sample(letters, len, replace = TRUE), collapse = "")
  }

# Make random data.
set.seed(1001)
d <- tibble::tibble(
  x = rnorm(10),
  y = rnorm(10),
  group = rep(c("A", "B"), c(5, 5)),
  lab = replicate(10, { random_string() })
)

d$xg <- d$x
d$xg[6:10] <- d$xg[6:10] + 1

test_that("stat_dens2d_filter_g, default arguments", {
  sd <- stat_dens2d_filter_g()
  expect_identical(sd$stat_params, stat_dens2d_filter_g()$stat_params)
})

test_that("stat_dens2d_filter_g, change default arguments", {
  sd1 <- stat_dens2d_filter_g(shape = 1, size = 3, keep.fraction = 1/4)
  expect_true(sd1$stat_params$keep.fraction == 1/4)
})

test_that("stat_dens2d_filter_g, incorrect argument", {
  expect_error(ggplot(data = d, aes(x, y, colour = group)) +
    geom_point() +
    stat_dens2d_filter_g(colour = "black",
                         shape = 1, size = 3, keep.fraction = 2))
})

test_that("stat_dens2d_filter_g, invalid keep.fraction arguments", {
  expect_error(stat_dens2d_filter_g(shape = 1, size = 3, keep.fraction = rep(0.1, 5), pool.along = "none"))
  expect_warning(stat_dens2d_filter_g(shape = 1, size = 3, keep.fraction = rep(0.1, 2), pool.along = "xy"))
  expect_warning(stat_dens2d_filter_g(shape = 1, size = 3, keep.fraction = rep(0.1, 3), pool.along = "x"))
  expect_warning(stat_dens2d_filter_g(shape = 1, size = 3, keep.fraction = rep(0.1, 3), pool.along = "y"))
})

test_that("stat_dens2d_filter_g, invalid keep.number arguments", {
  expect_error(stat_dens2d_filter_g(shape = 1, size = 3, keep.number = rep(2, 5), pool.along = "none"))
  expect_warning(stat_dens2d_filter_g(shape = 1, size = 3, keep.number = rep(2, 2), pool.along = "xy"))
  expect_warning(stat_dens2d_filter_g(shape = 1, size = 3, keep.number = rep(2, 3), pool.along = "x"))
  expect_warning(stat_dens2d_filter_g(shape = 1, size = 3, keep.number = rep(2, 3), pool.along = "y"))
})

