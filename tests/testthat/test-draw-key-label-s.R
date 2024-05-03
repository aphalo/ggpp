context("draw_key_label_s")

test_that("Returns expected value", {

  test.data <- data.frame(x = 1,
                          y = 5,
                          label = "a",
                          angle = 0,
                          colour = "red",
                          fill = "white",
                          alpha = 0.5,
                          vjust = 0.5,
                          hjust = 0.5)

  test.params.01 <- list(colour.target = "all",
                      alpha.target = "all",
                      default.colour = "black",
                      default.alpha = 1
                      )

  set.seed(1)
  obj.01 <- draw_key_label_s(data = test.data,
                             params = test.params.01,
                             size = 4)
  # expect_known_value(unname(obj.01),
  #                    "draw-key-label-s-01")
  expect_is(obj.01, "gTree")
  expect_length(obj.01, 5)
  expect_named(obj.01, c("name", "gp", "vp", "children", "childrenOrder"))


  test.params.02 <- list(colour.target = "text",
                      alpha.target = "none",
                      default.colour = "grey30",
                      default.alpha = 1
  )

  set.seed(1)
  obj.02 <- draw_key_label_s(data = test.data,
                             params = test.params.02,
                             size = 4)
  # expect_known_value(unname(obj.02),
  #                    "draw-key-label-s-02")
  expect_is(obj.02, "gTree")
  expect_length(obj.02, 5)
  expect_named(obj.02, c("name", "gp", "vp", "children", "childrenOrder"))

  test.params.03 <- list(colour.target = "text",
                         alpha.target = "none",
                         default.colour = "grey30",
                         default.alpha = 0
  )

  set.seed(1)
  obj.03 <- draw_key_label_s(data = test.data,
                             params = test.params.03,
                             size = 4)
  # expect_known_value(unname(obj.03),
  #                    "draw-key-label-s-03")
  expect_is(obj.03, "gTree")
  expect_length(obj.03, 5)
  expect_named(obj.03, c("name", "gp", "vp", "children", "childrenOrder"))

  test.params.04 <- list(colour.target = "box",
                         alpha.target = "none",
                         default.colour = "grey30",
                         default.alpha = 0
  )

  set.seed(1)
  obj.04 <- draw_key_label_s(data = test.data,
                             params = test.params.04,
                             size = 4)
  # expect_known_value(unname(obj.04),
  #                    "draw-key-label-s-04")
  expect_is(obj.04, "gTree")
  expect_length(obj.04, 5)
  expect_named(obj.04, c("name", "gp", "vp", "children", "childrenOrder"))

  test.params.05 <- list(colour.target = c("segment", "text"),
                         alpha.target = "none",
                         default.colour = "grey30",
                         default.alpha = 1
  )

  set.seed(1)
  obj.05 <- draw_key_label_s(data = test.data,
                             params = test.params.05,
                             size = 4)
  # expect_known_value(unname(obj.05),
  #                    "draw-key-label-s-05")
  expect_is(obj.05, "gTree")
  expect_length(obj.05, 5)
  expect_named(obj.05, c("name", "gp", "vp", "children", "childrenOrder"))



})

