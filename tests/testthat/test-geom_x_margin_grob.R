context("geom_x_margin_grob")

p <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point()

test_that("geom_x_margin_grob, missing xintercept", {
  p1 <- p + geom_x_margin_grob()
  result <- expect_error(layer_data(p1))
})
