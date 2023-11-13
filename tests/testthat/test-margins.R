test_that("justify_grobs() checks input", {
  expect_error(justify_grobs(1))
})

test_that("justify_grobs() passes input", {
  expect_no_error(justify_grobs(grid::nullGrob()))
  expect_no_error(justify_grobs(grid::nullGrob(), debug = TRUE))
})

test_that("rotate_just() accepts NULL", {
  expect_no_error(rotate_just(NULL, 0.5, 0.5))
})

test_that("rotate_just() works as expected", {
  expect_equal(rotate_just(NULL, 1, 1), list(hjust = 1, vjust = 1))
  expect_equal(rotate_just(45, 1, 1), list(hjust = 1, vjust = 1))
  expect_equal(rotate_just(135, 1, 1), list(hjust = 0, vjust = 1))
  expect_equal(rotate_just(225, 1, 1), list(hjust = 0, vjust = 0))
  expect_equal(rotate_just(315, 1, 1), list(hjust = 1, vjust = 0))
})
