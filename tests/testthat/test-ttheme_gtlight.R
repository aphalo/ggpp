context("ttheme_gtlight")

test_that("ttheme_gtlight, default text elements", {
  t1 <- ttheme_gtlight()

  expect_true(t1$core$fg_params$fontsize == 10)
  expect_true(t1$core$fg_params$col == "#1A1A1AFF")

  expect_identical(t1$core$bg_params$fill, ttheme_gtlight()$core$bg_params$fill)
  expect_identical(t1$core$bg_params$col, ttheme_gtlight()$core$bg_params$col)
  expect_identical(t1$core$bg_params$lwd, ttheme_gtlight()$core$bg_params$lwd)

  expect_identical(t1$colhead$bg_params$fill, ttheme_gtlight()$colhead$bg_params$fill)
  expect_identical(t1$colhead$bg_params$col, ttheme_gtlight()$colhead$bg_params$col)
  expect_identical(t1$colhead$bg_params$lwd, ttheme_gtlight()$colhead$bg_params$lwd)

  expect_identical(t1$rowhead$bg_params$fill, ttheme_gtlight()$rowhead$bg_params$fill)
  expect_identical(t1$rowhead$bg_params$col, ttheme_gtlight()$rowhead$bg_params$col)
  expect_identical(t1$rowhead$bg_params$lwd, ttheme_gtlight()$rowhead$bg_params$lwd)
})

test_that("ttheme_gtlight, check changes to text elements", {
  t2 <- ttheme_gtlight(base_size = 15)

  expect_true(t2$core$fg_params$fontsize == 15)
  expect_true(t2$core$fg_params$col == "#1A1A1AFF")

  expect_identical(t2$core$bg_params$fill, ttheme_gtlight()$core$bg_params$fill)
  expect_identical(t2$core$bg_params$col, ttheme_gtlight()$core$bg_params$col)
  expect_identical(t2$core$bg_params$lwd, ttheme_gtlight()$core$bg_params$lwd)

  expect_identical(t2$colhead$bg_params$fill, ttheme_gtlight()$colhead$bg_params$fill)
  expect_identical(t2$colhead$bg_params$col, ttheme_gtlight()$colhead$bg_params$col)
  expect_identical(t2$colhead$bg_params$lwd, ttheme_gtlight()$colhead$bg_params$lwd)

  expect_identical(t2$rowhead$bg_params$fill, ttheme_gtlight()$rowhead$bg_params$fill)
  expect_identical(t2$rowhead$bg_params$col, ttheme_gtlight()$rowhead$bg_params$col)
  expect_identical(t2$rowhead$bg_params$lwd, ttheme_gtlight()$rowhead$bg_params$lwd)
})
