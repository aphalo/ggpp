context("position_jitternudge")

test_that("incorrect kept.origin used", {
  expect_error(
    position_jitternudge(kept.origin = "wrong")
    #,  "`kept.origin` must be one of \"jittered\", \"original\", or \"none\", not\n\"wrong\"."
  )
})

test_that("correct kept.origin used", {
  position <- position_jitternudge(kept.origin = "jittered")
  expect_no_error(position)
})

test_that("incorrect nudge.from used", {
  expect_error(
    position_jitternudge(nudge.from = "original.z")
    #, "`nudge.from` must be one of \"original\", \"original.x\", \"original.y\",\n\"jittered\", \"jittered.y\", or \"jittered.x\", not \"original.z\".\nℹ Did you mean \"original.x\"?"
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

test_that("warn on x or y longer than data", {
  expect_no_warning(
    ggplot(mtcars, aes(x = am, y = mpg, label = mpg)) +
      geom_text_s(
        position = position_jitternudge(x = 0, y = rep(0.5, nrow(mtcars))),
        size = 2.5
      )
  )

  expect_no_warning(
    ggplot(mtcars, aes(x = am, y = mpg, label = mpg)) +
      geom_text_s(
        position = position_jitternudge(y = 0, x = rep(0.05, nrow(mtcars))),
        size = 2.5
      )
  )
})
