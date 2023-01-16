context("ttheme_gtsimple")

test_that("ttheme_gtsimple, default text elements", {
  t1 <- ttheme_gtsimple()

  expect_true(t1$core$fg_params$fontsize == 10)
  expect_true(t1$core$fg_params$col == "grey10")

  expect_identical(t1$core$bg_params$fill, ttheme_gtsimple()$core$bg_params$fill)
  expect_identical(t1$core$bg_params$col, ttheme_gtsimple()$core$bg_params$col)
  expect_identical(t1$core$bg_params$lwd, ttheme_gtsimple()$core$bg_params$lwd)

  expect_identical(t1$colhead$bg_params$fill, ttheme_gtsimple()$colhead$bg_params$fill)
  expect_identical(t1$colhead$bg_params$col, ttheme_gtsimple()$colhead$bg_params$col)
  expect_identical(t1$colhead$bg_params$lwd, ttheme_gtsimple()$colhead$bg_params$lwd)

  expect_identical(t1$rowhead$bg_params$fill, ttheme_gtsimple()$rowhead$bg_params$fill)
  expect_identical(t1$rowhead$bg_params$col, ttheme_gtsimple()$rowhead$bg_params$col)
  expect_identical(t1$rowhead$bg_params$lwd, ttheme_gtsimple()$rowhead$bg_params$lwd)
})

test_that("ttheme_gtsimple, check changes to text elements", {
  t2 <- ttheme_gtsimple(base_size = 20)

  expect_true(t2$core$fg_params$fontsize == 20)
  expect_true(t2$core$fg_params$col == "grey10")

  expect_identical(t2$core$bg_params$fill, ttheme_gtsimple()$core$bg_params$fill)
  expect_identical(t2$core$bg_params$col, ttheme_gtsimple()$core$bg_params$col)
  expect_identical(t2$core$bg_params$lwd, ttheme_gtsimple()$core$bg_params$lwd)

  expect_identical(t2$colhead$bg_params$fill, ttheme_gtsimple()$colhead$bg_params$fill)
  expect_identical(t2$colhead$bg_params$col, ttheme_gtsimple()$colhead$bg_params$col)
  expect_identical(t2$colhead$bg_params$lwd, ttheme_gtsimple()$colhead$bg_params$lwd)

  expect_identical(t2$rowhead$bg_params$fill, ttheme_gtsimple()$rowhead$bg_params$fill)
  expect_identical(t2$rowhead$bg_params$col, ttheme_gtsimple()$rowhead$bg_params$col)
  expect_identical(t2$rowhead$bg_params$lwd, ttheme_gtsimple()$rowhead$bg_params$lwd)
})
