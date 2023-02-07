context("geom_y_margin_grob")

p <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point()

test_that("geom_y_margin_grob, missing yintercept", {
  p1 <- p + geom_y_margin_grob()
  result <- expect_error(layer_data(p1))
})
