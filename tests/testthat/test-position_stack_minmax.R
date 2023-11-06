context("position_stack_minmax")

test_that("test vjust, reverse, x, y and kept.origin arguments", {
position <- position_stack_minmax()
expect_identical(position$vjust, 1)
expect_identical(position$reverse, FALSE)
expect_identical(position$kept.origin, "stacked")
expect_identical(position$var, "y")
expect_identical(position$fill, FALSE)
})
