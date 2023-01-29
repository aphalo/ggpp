context("compute_npcx")

test_that("character input value returns correct numeric value", {
  expect_equal(compute_npcx("right"), 0.95)
  expect_equal(compute_npcx("left"), 0.05)
  expect_equal(compute_npcx(c("left", "right")), c(0.05, 0.95))
  expect_equal(compute_npcx("centre"), 0.5)
  expect_equal(compute_npcx("center"), 0.5)
  expect_equal(compute_npcx("middle"), 0.5)
  expect_equal(compute_npcx("bottom"), NA)
})

test_that("factor input values returns correct numeric value", {
  x <- factor(c("right", "left"))
  expect_equal(compute_npcx(x), c(0.95, 0.05))

})

test_that("numeric input value returns the same value if
          between 0 and 1", {
  x <- c(0.5, 1)
  expect_equal(compute_npcx(x), x)
})

test_that("numeric input value
          returns 0 if less than 0,
          returns 1 if greater than 1", {
            x <- c(-0.5, 2)
            expect_equal(compute_npcx(x), c(0,1))
          })
