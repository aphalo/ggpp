context("stat_summary_xy")

library(ggplot2)

test_that("stat_summary_xy", {
  tst.df <- data.frame(
    x = c(2.2, -0.2, -0.2, -2.5, -0.6, -0.1),
    y = c(1.1, -0.6, -0.9, -1.6, 0.3, 1.6),
    group = c("A", "A", "A", "B", "B", "B")
  )
    result <- ggplot(tst.df, aes(x, y)) +
              stat_summary_xy()
    expected <-ggplot2::layer_data(result)

    result <- ggplot(tst.df, aes(x, y)) +
      stat_summary_xy(.fun.x = mean)
    result <- ggplot2::layer_data(result)
    result <- result$x
    expected <- mean(tst.df$x)
    expect_identical(result, expected)

    result <- ggplot(tst.df, aes(x, y)) +
      stat_summary_xy(.fun.y = mean)
    result <- ggplot2::layer_data(result)
    result <- result$y
    expected <- mean(tst.df$y)
    expect_identical(result, expected)

    result <- ggplot(tst.df, aes(x, y)) +
      stat_summary_xy(.fun.y = mean_se)
    result <- ggplot2::layer_data(result)
    result <- result$y
    expected <- mean_se(tst.df$y)
    expected <- expected$y
    expect_identical(result, expected)

    function_passed <- "mean_se"
    result <- ggplot(tst.df, aes(x, y)) +
      stat_summary_xy(.fun.y = function_passed)
    result <- ggplot2::layer_data(result)
    result <- result$y
    expected <- mean_se(tst.df$y)
    expected <- expected$y
    expect_identical(result, expected)

  })
