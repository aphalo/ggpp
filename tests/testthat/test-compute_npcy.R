context("compute_npcy")

test_that("character input value returns correct numeric value", {
  expect_equal(compute_npcy("top"), 0.95)
  expect_equal(compute_npcy("bottom"), 0.05)
  expect_equal(compute_npcy("bottom", group = 1L:3L), c(0.05, 0.15, 0.25))
  expect_equal(compute_npcy("bottom", group = 2L), 0.15)
  expect_equal(compute_npcy("center"), 0.5)
  expect_equal(compute_npcy("middle"), 0.5)
  expect_equal(compute_npcy("left"), NA)
})

test_that("factor input values returns correct numeric value", {
  y <- factor(c("top", "bottom"))
  expect_equal(compute_npcy(y), c(0.95, 0.05))
})


test_that("numeric input values returns the same value if
          between 0 and 1", {
            y <- c(0.5, 1)
            expect_equal(compute_npcy(y), y)
          })

test_that("numeric input values
          returns 0 if less than 0,
          returns 1 if greater than 1", {
            y <- c(-0.5, 2)
            expect_equal(compute_npcy(y), c(0,1))
          })

