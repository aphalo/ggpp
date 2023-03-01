context("stat_fmt_tb")

test_that("stat_fmt_tb", {
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
    expand_limits(x = c(0,3), y = c(-2, 6)) +
    geom_point()
  expect_s3_class(result, "ggplot")

  expect_silent(result <- ggplot(my.df, aes(x, y, label = tbs)) +
    stat_fmt_tb() +
    expand_limits(x = c(0,3), y = c(-2, 6)))
  result <- ggplot2::layer_data(result,1)
  result1 <- result$label[[1]]
  expected <- tibble::tibble(Xa = as.numeric(1:6), Y = rep(c("x", "y"), 3))
  expect_identical(result1, expected)

  result2 <- result$label[[2]]
  expected <-tibble::tibble(Xb = as.numeric(1:3), Y = "x")
  expect_identical(result2, expected)

  # Hide column names, diplay row names
  expect_silent(result2 <- ggplot(my.df, aes(x, y, label = tbs)) +
    stat_fmt_tb(table.colnames = FALSE,
                table.rownames = TRUE))

  # Use a theme for the table
  ggplot(my.df, aes(x, y, label = tbs)) +
    stat_fmt_tb(table.theme = ttheme_gtlight) +
    expand_limits(x = c(0,3), y = c(-2, 6))

  vdiffr::expect_doppelganger("stat_fmt_tb_1",
                              ggplot(my.df, aes(x, y, label = tbs)) +
                                stat_fmt_tb(table.theme = ttheme_gtlight) +
                                expand_limits(x = c(0,4), y = c(-2, 6))
  )

  # selection and renaming by column position
  vdiffr::expect_doppelganger("stat_fmt_tb_2",
                              ggplot(my.df, aes(x, y, label = tbs)) +
                              stat_fmt_tb(tb.vars = c(value = 1, group = 2),
                                           tb.rows = 1:3) +
                              expand_limits(x = c(0,3), y = c(-2, 6)))


  # selection, reordering and renaming by column position
  vdiffr::expect_doppelganger("stat_fmt_tb_3",
                              ggplot(my.df, aes(x, y, label = tbs)) +
                              stat_fmt_tb(tb.vars = c(group = 2, value = 1),
                                          tb.rows = 1:3) +
                              expand_limits(x = c(0,3), y = c(-2, 6)))

  # selection and renaming, using partial matching to column name
  vdiffr::expect_doppelganger("stat_fmt_tb_4",
                              ggplot(my.df, aes(x, y, label = tbs)) +
                              stat_fmt_tb(tb.vars = c(value = "X", group = "Y"),
                                          tb.rows = 1:3) +
                              expand_limits(x = c(0,3), y = c(-2, 6)))
})
