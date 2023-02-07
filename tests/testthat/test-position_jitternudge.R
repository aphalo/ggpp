context("position_jitternudge")

test_that("incorrect kept.origin used", {
  expect_error(
    position_jitternudge(kept.origin = "origin"),
    "Invalid 'kept.origin': 'origin, expected: `\"original\", \"jittered\" or \"none\""
  )
})

test_that("correct kept.origin used", {
  position <- position_jitternudge(kept.origin = "jittered")
  expect_no_error(position)
})

test_that("incorrect nudge.from used", {
  expect_error(
    position_jitternudge(nudge.from = "original.z"),
    "Invalid 'nudge.from': 'original.z', expected: '\"original\", \"original.x\", \"original.y\" or \"jittered\""
  )
})

test_that("correct nudge.from used", {
  position <- position_jitternudge(nudge.from = "original.x")
  expect_no_error(position)
})

test_that("test if correct arguments are assigned", {
  position <- position_jitternudge(width = 0.2, height = 2,
                                   seed = 123, x = 0.35,
                                   direction = "split",
                                   nudge.from = "original.x")

  expect_identical(position$kept.origin, "jittered")
  expect_identical(position$nudge.from, "original.x")
  expect_identical(position$width, 0.2)
  expect_identical(position$height, 2)
  expect_identical(position$seed, 123)

})
