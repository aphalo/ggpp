context("geom_y_margin_point")

test_that("geom_y_margin_point, missing yintercept", {
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point()
  p1 <- p + geom_y_margin_point()

  result <- expect_error(layer_data(p1))
})

test_that("geom_y_margin_point, yintercept", {
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point()
  p1 <- p + geom_y_margin_point(yintercept = c(18, 28, 15))

  expect_true(p1$layers[[2]]$data$yintercept[1] == 18)
  expect_true(p1$layers[[2]]$data$yintercept[2] == 28)
  expect_true(p1$layers[[2]]$data$yintercept[3] == 15)
})

test_that("geom_y_margin_point, yintercept dataframe", {
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point()
  p1 <- p + geom_y_margin_point(data = data.frame(x = c(2.5, 4.5)),
                                mapping = aes(yintercept = x))

  expect_true(p1$layers[[2]]$data$x[1] == 2.5)
  expect_true(p1$layers[[2]]$data$x[2] == 4.5)
})
