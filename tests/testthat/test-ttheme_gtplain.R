context("ttheme_gtplain")

test_that("ttheme_gtplain, default text elements", {
  t1 <- ttheme_gtplain()

  expect_true(t1$core$fg_params$fontsize == 10)
  expect_true(t1$core$fg_params$col == "#000000FF")

  expect_identical(t1$core$bg_params$fill, ttheme_gtplain()$core$bg_params$fill)

  expect_identical(t1$colhead$bg_params$fill, ttheme_gtplain()$colhead$bg_params$fill)

  expect_identical(t1$rowhead$bg_params$fill, ttheme_gtplain()$rowhead$bg_params$fill)
})

test_that("ttheme_gtplain, check changes to text elements", {
  t2 <- ttheme_gtplain(base_size = 15, base_colour = "blue")

  expect_true(t2$core$fg_params$fontsize == 15)
  expect_true(t2$core$fg_params$col == "#0000FFFF")

  expect_identical(t2$core$bg_params$fill, ttheme_gtplain()$core$bg_params$fill)

  expect_identical(t2$colhead$bg_params$fill, ttheme_gtplain()$colhead$bg_params$fill)

  expect_identical(t2$rowhead$bg_params$fill, ttheme_gtplain()$rowhead$bg_params$fill)
})
