context("geom_y_margin_arrow")

test_that("geom_y_margin_arrow, missing yintercept", {
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point()
  p1 <- p + geom_y_margin_arrow()

  result <- expect_error(layer_data(p1))
})

test_that("geom_y_margin_arrow, yintercept", {
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point()
  p1 <- p + geom_y_margin_arrow(yintercept = c(18, 28, 15))

  expect_true(p1$layers[[2]]$data$yintercept[1] == 18)
  expect_true(p1$layers[[2]]$data$yintercept[2] == 28)
  expect_true(p1$layers[[2]]$data$yintercept[3] == 15)
})

test_that("geom_y_margin_arrow, yintercept dataframe", {
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point()
  p1 <- p + geom_y_margin_arrow(data = data.frame(x = c(2.5, 4.5)),
                                mapping = aes(yintercept = x))

  expect_true(p1$layers[[2]]$data$x[1] == 2.5)
  expect_true(p1$layers[[2]]$data$x[2] == 4.5)
})
