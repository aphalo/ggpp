context("draw_key_text_s")

test_that("Returns expected value", {

  test.data <- data.frame(x = 1,
                          y = 5,
                          label = "a",
                          angle = 0,
                          colour = "red",
                          alpha = 0.5,
                          vjust = 0.5,
                          hjust = 0.5)

  test.params.01 <- list(colour.target = "all",
                         alpha.target = "all",
                         default.colour = "black",
                         default.alpha = 1
  )

  # names of grob members seem to change randomly from run to run
  # set.seed() and unname() do not seem to help!
  set.seed(1)
  obj.01 <- draw_key_text_s(data = test.data,
                            params = test.params.01,
                            size = 4)
  # expect_known_value(unname(obj.01),
  #                    "draw-key-text-s-01")
  expect_is(obj.01, "titleGrob")
  expect_length(obj.01, 7)
  expect_named(obj.01, c("widths", "heights", "name", "gp", "vp", "children", "childrenOrder"))

  test.params.02 <- list(colour.target = "text",
                      alpha.target = "none",
                      default.colour = "grey30",
                      default.alpha = 1
  )

  set.seed(1)
  obj.02 <- draw_key_text_s(data = test.data,
                            params = test.params.02,
                            size = 4)
  # expect_known_value(unname(obj.02),
  #                    "draw-key-text-s-02")
  expect_is(obj.02, "titleGrob")
  expect_length(obj.02, 7)
  expect_named(obj.02, c("widths", "heights", "name", "gp", "vp", "children", "childrenOrder"))

  test.params.03 <- list(colour.target = "text",
                         alpha.target = "none",
                         default.colour = "grey30",
                         default.alpha = 0
  )

  set.seed(1)
  obj.03 <- draw_key_text_s(data = test.data,
                            params = test.params.03,
                            size = 4)
  # expect_known_value(unname(obj.03),
  #                    "draw-key-text-s-03")
  expect_is(obj.03, "titleGrob")
  expect_length(obj.03, 7)
  expect_named(obj.03, c("widths", "heights", "name", "gp", "vp", "children", "childrenOrder"))

})

