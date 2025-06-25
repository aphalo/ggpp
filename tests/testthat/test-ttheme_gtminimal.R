context("ttheme_gtminimal")

test_that("ttheme_gtminimal, default text elements", {
  t1 <- ttheme_gtminimal()

  expect_true(t1$core$fg_params$fontsize == 10)
  expect_true(t1$core$fg_params$col == "#000000FF")
})

test_that("ttheme_gtminimal, check changes to text elements", {
  t2 <- ttheme_gtminimal(base_size = 15, base_colour = "blue")

  expect_true(t2$core$fg_params$fontsize == 15)
  expect_true(t2$core$fg_params$col == "#0000FFFF")
})
