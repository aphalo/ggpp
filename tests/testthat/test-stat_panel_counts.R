context("stat_panel_counts")

test_that("stat_panel_counts", {
  tst.df <- data.frame(
    x = c(2.2, -0.2, -0.2, -2.5, -0.6, -0.1),
    y = c(1.1, -0.6, -0.9, -1.6, 0.3, 1.6),
    group = c("A", "A", "A", "B", "B", "B")
  )
  vdiffr::expect_doppelganger("stat_panel_counts_xy",
                              ggplot(tst.df, aes(x, y)) +
                                stat_panel_counts() +
                                geom_point()
                              )

  vdiffr::expect_doppelganger("stat_panel_counts_x",
                              ggplot(tst.df, aes(x)) +
                                stat_panel_counts() +
                                geom_histogram(bins = 3)
                              )

  vdiffr::expect_doppelganger("stat_panel_counts_y",
                              ggplot(tst.df, aes(y)) +
                                stat_panel_counts() +
                                geom_histogram(bins = 3)
                              )

  p <- ggplot(tst.df, aes(x, y)) +
    stat_panel_counts() +
    geom_point()
  result <- layer_data(p)[, c("npcx", "npcy", "label", "count", "hjust", "vjust")]
  expected <- data.frame(
    npcx = 0.95,
    npcy = 0.95,
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


test_that("stat_group_counts", {
  tst.df <- data.frame(
    x = c(2.2, -0.2, -0.2, -2.5, -0.6, -0.1),
    y = c(1.1, -0.6, -0.9, -1.6, 0.3, 1.6),
    group = c("A", "A", "A", "B", "B", "B")
  )
  vdiffr::expect_doppelganger("stat_group_counts_xy",
                              ggplot(tst.df, aes(x, y)) +
                                stat_group_counts() +
                                geom_point()
  )

  vdiffr::expect_doppelganger("stat_group_counts_xy_color",
                              ggplot(tst.df, aes(x, y, color = group)) +
                                stat_group_counts() +
                                geom_point()
  )

  vdiffr::expect_doppelganger("stat_group_counts_x",
                              ggplot(tst.df, aes(x)) +
                                stat_group_counts() +
                                geom_histogram(bins = 3)
  )

  vdiffr::expect_doppelganger("stat_group_counts_y",
                              ggplot(tst.df, aes(y)) +
                                stat_group_counts() +
                                geom_histogram(bins = 3)
  )

  p <- ggplot(tst.df, aes(x, y)) +
    stat_group_counts() +
    geom_point()
  result <- layer_data(p)[, c("npcx", "npcy", "label", "count", "total",
                              "count.label", "pc.label", "dec.label", "fr.label", "hjust", "vjust")]
  expected <- data.frame(
    npcx = 0.95,
    npcy = 0.95,
    label = "n=6",
    count = 6L,
    total = 6L,
    count.label = "n=6",
    pc.label = "p=100%",
    dec.label = "f=1.00",
    fr.label = "6 / 6",
    hjust = "inward",
    vjust = "inward")
  expect_identical(result, expected)

  p <- ggplot(tst.df, aes(x)) +
    stat_group_counts()
  result <- layer_data(p)[, c("npcx", "npcy", "label", "count", "total",
                              "count.label", "pc.label", "dec.label", "fr.label", "hjust", "vjust")]
  expect_identical(result, expected)

  p <- ggplot(tst.df, aes(y)) +
    stat_group_counts()
  result <- layer_data(p)[, c("npcx", "npcy", "label", "count", "total",
                              "count.label", "pc.label", "dec.label", "fr.label", "hjust", "vjust")]
  expect_identical(result, expected)

  p <- ggplot(tst.df, aes(x, y)) +
    stat_group_counts(digits = 3) +
    geom_point()
  result <- layer_data(p)[, c("npcx", "npcy", "label", "count", "total",
                              "count.label", "pc.label", "dec.label", "fr.label", "hjust", "vjust")]
  expected <- data.frame(
    npcx = 0.95,
    npcy = 0.95,
    label = "n=6",
    count = 6L,
    total = 6L,
    count.label = "n=6",
    pc.label = "p=100.0%",
    dec.label = "f=1.000",
    fr.label = "6 / 6",
    hjust = "inward",
    vjust = "inward")
  expect_identical(result, expected)

  expect_error(ggplot(tst.df, aes(x, y)) +
                 stat_group_counts(label.x = NA))

  expect_error(ggplot(tst.df, aes(x, y)) +
                 stat_group_counts(label.y = NA))

  expect_error(ggplot(tst.df, aes(x, y, color = group)) +
                 stat_group_counts(label.x = NA))

  expect_error(ggplot(tst.df, aes(x, y, color = group)) +
                 stat_group_counts(label.y = NA))

})
