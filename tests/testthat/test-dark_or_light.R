context("dark_or_light")

test_that("character(0) on NULL input", {
  res <- dark_or_light(NULL)
  expect_equal(res, character(0))
})

test_that("character(0) on character(0) input", {
  res <- dark_or_light(character(0))
  expect_equal(res, character(0))
})

test_that("color: yellow, switch to black, default threshold", {
  res <- dark_or_light("yellow")
  expect_equal(res, "black")
})

test_that("color: darkblue, switch to white, default threshold", {
  res <- dark_or_light("darkblue")
  expect_equal(res, "white")
})

test_that("color: #FFFFFF, switch to black, default threshold", {
  res <- dark_or_light("#FFFFFF")
  expect_equal(res, "black")
})

test_that("color: #000000, switch to white, default threshold", {
  res <- dark_or_light("#000000")
  expect_equal(res, "white")
})

test_that("color: #000000, switch to light.color specified, default threshold", {
  res <- dark_or_light("#000000", dark.color = "darkblue", light.color = "lightgrey")
  expect_equal(res, "lightgrey")
})

test_that("color: #000000, switch to dark.color specified, default threshold", {
  res <- dark_or_light("#FFFFFF", dark.color = "darkblue", light.color = "lightgrey")
  expect_equal(res, "darkblue")
})

test_that("color: yellow, threshold 0.8, switch to white", {
  res <- dark_or_light("yellow", threshold = 0.8)
  expect_equal(res, "white")
})

test_that("threshold: off range triggers error", {
  expect_no_error(dark_or_light("yellow", threshold = 0))
  expect_no_error(dark_or_light("yellow", threshold = 1))
  expect_error(dark_or_light("yellow", threshold = -0.1))
  expect_error(dark_or_light("yellow", threshold = 1.1))
})

test_that("threshold: length != 1 triggers error", {
  expect_error(dark_or_light("yellow", threshold = numeric()))
  expect_error(dark_or_light("yellow", threshold = NULL))
  expect_error(dark_or_light("yellow", threshold = c(0.45, 0.45)))
})

