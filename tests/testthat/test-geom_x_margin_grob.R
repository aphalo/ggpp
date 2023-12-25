context("geom_x_margin_grob")


p <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point()

test_that("geom_x_margin_grob, missing xintercept", {
  p1 <- p + geom_x_margin_grob()
  expect_error(layer_data(p1))
})

test_that("geom_x_margin_grob, not missing xintercept", {
  p1 <- p + geom_x_margin_grob(xintercept = 1)
  expect_no_error(layer_data(p1))
})
