context("geom_x_margin_point")

test_that("geom_x_margin_point, missing xintercept", {
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point()
  p1 <- p + geom_x_margin_point()

  result <- expect_error(layer_data(p1))
})

test_that("geom_x_margin_point, xintercept single value", {
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point()
  p1 <- p + geom_x_margin_point(xintercept = 3.5)

  expect_true(p1$layers[[2]]$data$xintercept == 3.5)
})

test_that("geom_x_margin_point, xintercept dataframe", {
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point()
  p1 <- p + geom_x_margin_point(data = data.frame(x = c(2.5, 4.5)),
                                mapping = aes(xintercept = x))

  expect_true(p1$layers[[2]]$data$x[1] == 2.5)
  expect_true(p1$layers[[2]]$data$x[2] == 4.5)
})


