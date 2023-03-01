context("stat_fmt_tb")

test_that("stat_fmt_tb", {
  #data frame to test with

  #results of using function

  #expected result via another method

  #expect_equal(result, expected)

  #other expect methods

  #code from github
  my.df <-
    tibble::tibble(
      x = c(1, 2),
      y = c(0, 4),
      group = c("A", "B"),
      tbs = list(a = tibble::tibble(Xa = 1:6, Y = rep(c("x", "y"), 3)),
                 b = tibble::tibble(Xb = 1:3, Y = "x"))
    )

  result <- ggplot(my.df, aes(x, y, label = tbs)) +
    stat_fmt_tb() +
    expand_limits(x = c(0,3), y = c(-2, 6))
  expect_s3_class(result, "ggplot")

  result <- ggplot(my.df, aes(x, y, label = tbs)) +
    stat_fmt_tb() +
    expand_limits(x = c(0,3), y = c(-2, 6))
  result <- ggplot2::layer_data(result,2)
  result <- result[ ,c("x","y")]
  expected <- data.frame(x = c(0,3), y = c(-2,6))
  expect_identical(result, expected)

  # Hide column names, diplay row names
  result2 <- ggplot(my.df, aes(x, y, label = tbs)) +
    stat_fmt_tb(table.colnames = FALSE,
                table.rownames = TRUE)
  ggplot2::layer_data(result2, 1) %>% colnames()
  ggplot2::layer_data(result2, 1) %>% rownames()

  # Use a theme for the table
  ggplot(my.df, aes(x, y, label = tbs)) +
    stat_fmt_tb(table.theme = ttheme_gtlight) +
    expand_limits(x = c(0,3), y = c(-2, 6))

  # selection and renaming by column position
  ggplot(my.df, aes(x, y, label = tbs)) +
    stat_fmt_tb(tb.vars = c(value = 1, group = 2),
                 tb.rows = 1:3) +
    expand_limits(x = c(0,3), y = c(-2, 6))

  # selection, reordering and renaming by column position
  ggplot(my.df, aes(x, y, label = tbs)) +
    stat_fmt_tb(tb.vars = c(group = 2, value = 1),
                tb.rows = 1:3) +
    expand_limits(x = c(0,3), y = c(-2, 6))

  # selection and renaming, using partial matching to column name
  ggplot(my.df, aes(x, y, label = tbs)) +
    stat_fmt_tb(tb.vars = c(value = "X", group = "Y"),
                tb.rows = 1:3) +
    expand_limits(x = c(0,3), y = c(-2, 6))
})
