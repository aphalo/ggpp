context("stat_panel_counts")

test_that("stat_panel_counts", {
  tst.df <- data.frame(
    x = c(2.2, -0.2, -0.2, -2.5, -0.6, -0.1),
    y = c(1.1, -0.6, -0.9, -1.6, 0.3, 1.6),
    group = c("A", "A", "A", "B", "B", "B")
  )
  vdiffr::expect_doppelganger("stat_quadrant_counts_xy",
                              ggplot(tst.df, aes(x, y)) +
                                stat_panel_counts())

  vdiffr::expect_doppelganger("stat_quadrant_counts_x",
                              ggplot(tst.df, aes(x)) +
                                stat_panel_counts())

  vdiffr::expect_doppelganger("stat_quadrant_counts_y",
                              ggplot(tst.df, aes(y)) +
                                stat_panel_counts())

  p <- ggplot(tst.df, aes(x, y)) +
    stat_panel_counts()
  result <- layer_data(p)[, c("npcx", "npcy", "label", "count", "hjust", "vjust")]
  expected <- data.frame(
    npcx = "right",
    npcy = "top",
    label = "n=6",
    count = 6L,
    hjust = "inward",
    vjust = "inward")
  expect_identical(result, expected)

  p <- ggplot(tst.df, aes(x)) +
    stat_panel_counts()
  result <- layer_data(p)[, c("npcx", "npcy", "label", "count", "hjust", "vjust")]
  expect_identical(result, expected)

  p <- ggplot(tst.df, aes(y)) +
    stat_panel_counts()
  result <- layer_data(p)[, c("npcx", "npcy", "label", "count", "hjust", "vjust")]
  expect_identical(result, expected)

  expect_error(ggplot(tst.df, aes(x, y)) +
      stat_panel_counts(label.x = NA))

  expect_error(ggplot(tst.df, aes(x, y)) +
      stat_panel_counts(label.y = NA))

  })
