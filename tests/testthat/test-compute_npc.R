context("compute_npc")

test_that("character input value returns correct numeric value", {
  expect_equal(compute_npc("right"), 0.95)
  expect_equal(compute_npc("left"), 0.05)
  expect_equal(compute_npc(c("left", "right")), c(0.05, 0.95))
  expect_equal(compute_npc("centre"), 0.5)
  expect_equal(compute_npc("center"), 0.5)
  expect_equal(compute_npc("middle"), 0.5)
  expect_equal(compute_npc("bottom"), 0.05)
  expect_equal(compute_npc("top"), 0.95)
})

test_that("factor input values returns correct numeric value", {
  x <- factor(c("right", "left"))
  expect_equal(compute_npc(x), c(0.95, 0.05))

})

test_that("numeric input value returns the same value if
          between 0 and 1", {
  x <- c(0.5, 1)
  expect_equal(compute_npc(x), x)
})

test_that("numeric input value
          returns 0 if less than 0,
          returns 1 if greater than 1", {
            x <- c(-0.5, 2)
            expect_equal(compute_npc(x), c(0,1))
          })

test_that("Value is returned 'AsIs'", {
  expect_is(as_npc("right"), "AsIs")
  expect_equal(as_npc("right"), I(0.95))
  expect_equal(as_npc("left"), I(0.05))
  expect_equal(as_npc("centre"), I(0.5))
  expect_equal(as_npc("center"), I(0.5))
})
