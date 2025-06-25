context("ttheme_gtstripes")

test_that("ttheme_gtstripes, default text elements", {
  t1 <- ttheme_gtstripes()

  expect_true(t1$core$fg_params$fontsize == 10)
  expect_true(t1$core$fg_params$col == "#1A1A1AFF")

  expect_identical(t1$core$bg_params$fill, ttheme_gtstripes()$core$bg_params$fill)
  expect_identical(t1$core$bg_params$col, ttheme_gtstripes()$core$bg_params$col)
  expect_identical(t1$core$bg_params$lwd, ttheme_gtstripes()$core$bg_params$lwd)

  expect_identical(t1$colhead$bg_params$fill, ttheme_gtstripes()$colhead$bg_params$fill)
  expect_identical(t1$colhead$bg_params$col, ttheme_gtstripes()$colhead$bg_params$col)
  expect_identical(t1$colhead$bg_params$lwd, ttheme_gtstripes()$colhead$bg_params$lwd)

  expect_identical(t1$rowhead$bg_params$fill, ttheme_gtstripes()$rowhead$bg_params$fill)
  expect_identical(t1$rowhead$bg_params$col, ttheme_gtstripes()$rowhead$bg_params$col)
  expect_identical(t1$rowhead$bg_params$lwd, ttheme_gtstripes()$rowhead$bg_params$lwd)
})

test_that("ttheme_gtstripes, check changes to text elements", {
  t2 <- ttheme_gtstripes(base_size = 12, base_colour = "darkred")

  expect_true(t2$core$fg_params$fontsize == 12)
  expect_true(t2$core$fg_params$col == "#8B0000FF")

  expect_identical(t2$core$bg_params$fill, ttheme_gtstripes()$core$bg_params$fill)
  expect_identical(t2$core$bg_params$col, ttheme_gtstripes()$core$bg_params$col)
  expect_identical(t2$core$bg_params$lwd, ttheme_gtstripes()$core$bg_params$lwd)

  expect_identical(t2$colhead$bg_params$fill, ttheme_gtstripes()$colhead$bg_params$fill)
  expect_identical(t2$colhead$bg_params$col, ttheme_gtstripes()$colhead$bg_params$col)
  expect_identical(t2$colhead$bg_params$lwd, ttheme_gtstripes()$colhead$bg_params$lwd)

  expect_identical(t2$rowhead$bg_params$fill, ttheme_gtstripes()$rowhead$bg_params$fill)
  expect_identical(t2$rowhead$bg_params$col, ttheme_gtstripes()$rowhead$bg_params$col)
  expect_identical(t2$rowhead$bg_params$lwd, ttheme_gtstripes()$rowhead$bg_params$lwd)
})

