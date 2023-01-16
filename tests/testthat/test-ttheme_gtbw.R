context("ttheme_gtbw")

test_that("ttheme_gtbw, default text elements", {
  t1 <- ttheme_gtbw()

  expect_true(t1$core$fg_params$fontsize == 10)
  expect_true(t1$core$fg_params$col == "black")

  expect_identical(t1$core$bg_params$fill, ttheme_gtbw()$core$bg_params$fill)
  expect_identical(t1$core$bg_params$col, ttheme_gtbw()$core$bg_params$col)
  expect_identical(t1$core$bg_params$lwd, ttheme_gtbw()$core$bg_params$lwd)

  expect_identical(t1$colhead$bg_params$fill, ttheme_gtbw()$colhead$bg_params$fill)
  expect_identical(t1$colhead$bg_params$col, ttheme_gtbw()$colhead$bg_params$col)
  expect_identical(t1$colhead$bg_params$lwd, ttheme_gtbw()$colhead$bg_params$lwd)

  expect_identical(t1$rowhead$bg_params$fill, ttheme_gtbw()$rowhead$bg_params$fill)
  expect_identical(t1$rowhead$bg_params$col, ttheme_gtbw()$rowhead$bg_params$col)
  expect_identical(t1$rowhead$bg_params$lwd, ttheme_gtbw()$rowhead$bg_params$lwd)
})

test_that("ttheme_gtbw, check changes to text elements", {
  t2 <- ttheme_gtbw(base_size = 15, base_colour = "darkblue")

  expect_true(t2$core$fg_params$fontsize == 15)
  expect_true(t2$core$fg_params$col == "darkblue")

  expect_identical(t2$core$bg_params$fill, ttheme_gtbw()$core$bg_params$fill)
  expect_identical(t2$core$bg_params$col, ttheme_gtbw()$core$bg_params$col)
  expect_identical(t2$core$bg_params$lwd, ttheme_gtbw()$core$bg_params$lwd)

  expect_identical(t2$colhead$bg_params$fill, ttheme_gtbw()$colhead$bg_params$fill)
  expect_identical(t2$colhead$bg_params$col, ttheme_gtbw()$colhead$bg_params$col)
  expect_identical(t2$colhead$bg_params$lwd, ttheme_gtbw()$colhead$bg_params$lwd)

  expect_identical(t2$rowhead$bg_params$fill, ttheme_gtbw()$rowhead$bg_params$fill)
  expect_identical(t2$rowhead$bg_params$col, ttheme_gtbw()$rowhead$bg_params$col)
  expect_identical(t2$rowhead$bg_params$lwd, ttheme_gtbw()$rowhead$bg_params$lwd)
})
