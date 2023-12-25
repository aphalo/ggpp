context("geom_y_margin_grob")

p <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point()

test_that("geom_y_margin_grob, missing yintercept", {
  p1 <- p + geom_y_margin_grob()
  result <- expect_error(layer_data(p1))
})

test_that("geom_y_margin_grob, not missing yintercept", {
  p1 <- p + geom_y_margin_grob(yintercept = 1)
  expect_no_error(layer_data(p1))
})
