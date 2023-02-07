context("stat_dens1d_filter_g")

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

test_that("stat_dens1d_filter_g, default arguments", {
  sd <- stat_dens1d_filter_g()
  expect_identical(sd$stat_params, stat_dens1d_filter_g()$stat_params)
})

test_that("stat_dens1d_filter_g, change default arguments", {
  sd1 <- stat_dens1d_filter_g(shape = 1, size = 3, keep.fraction = 1/4, adjust = 2)
  expect_true(sd1$stat_params$keep.fraction == 1/4)
  expect_true(sd1$stat_params$adjust == 2)

})

test_that("stat_dens1d_filter_g, incorrect argument", {
  expect_error(ggplot(data = d, aes(xg, y, colour = group)) +
    geom_point() +
    geom_rug(sides = "b") +
    stat_dens1d_filter_g(colour = "black",
                         shape = 1, size = 3, keep.fraction = 2, adjust = 2))
})
