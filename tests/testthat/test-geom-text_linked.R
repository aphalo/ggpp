context("geom_text_s")
# Here we not only test geom_text_s() but also function compute_just2d()
# which translates character strings into numeric values of justification
# taking into account angle of rotation direction splits.

test_that("text_linked_just", {
  df <- data.frame(
    x = c(1, 1, 2, 2, 1.5),
    y = c(1, 2, 1, 2, 1.5),
    text = c("bottom-left", "top-left", "bottom-right", "top-right", "center")
  )

  vdiffr::expect_doppelganger("geom_text_s1",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text))
  )

  vdiffr::expect_doppelganger("geom_text_s2",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text),
                                            vjust = "inward", hjust = "inward")
  )
  vdiffr::expect_doppelganger("geom_text_s3",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text),
                                            hjust = "inward", angle = 33)
  )
  vdiffr::expect_doppelganger("geom_text_s4",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text),
                                            hjust = "inward", angle = 66)
  )
  vdiffr::expect_doppelganger("geom_text_s5",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text),
                                            hjust = "inward", angle = 90)
  )
  vdiffr::expect_doppelganger("geom_text_s7",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text),
                                            vjust = "inward",
                                            hjust = "inward", angle = 33)
  )
  vdiffr::expect_doppelganger("geom_text_s8",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text),
                                            vjust = "inward",
                                            hjust = "inward", angle = 66)
  )
  vdiffr::expect_doppelganger("geom_text_s9",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text),
                                            vjust = "inward",
                                            hjust = "inward", angle = 90)
  )
  vdiffr::expect_doppelganger("geom_text_s10",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text),
                                            vjust = "inward",
                                            hjust = "inward", angle = 90)
  )
  vdiffr::expect_doppelganger("geom_text_s11",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text),
                                            vjust = "inward_1.5",
                                            hjust = "inward_1.5")
  )
  vdiffr::expect_doppelganger("geom_text_s12",
                              ggplot(df, aes(x - 1.5, y - 1.5)) +
                                geom_text_s(aes(label = text),
                                            vjust = "inward_0.0",
                                            hjust = "inward_0.0")
  )
  vdiffr::expect_doppelganger("geom_text_s13",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text),
                                            vjust = "inward_mean",
                                            hjust = "inward_mean")
  )
  vdiffr::expect_doppelganger("geom_text_s14",
                              ggplot(df, aes(x - 1.5, y - 1.5)) +
                                geom_text_s(aes(label = text),
                                            vjust = "inward_mean",
                                            hjust = "inward_mean")
  )
  vdiffr::expect_doppelganger("geom_text_s15",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text),
                                            vjust = "inward_median",
                                            hjust = "inward_median")
  )

  vdiffr::expect_doppelganger("geom_text_s16",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text),
                                            vjust = "outward",
                                            hjust = "outward")
  )
  vdiffr::expect_doppelganger("geom_text_s17",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text),
                                            vjust = "outward_mean",
                                            hjust = "outward_mean")
  )
  vdiffr::expect_doppelganger("geom_text_s18",
                              ggplot(df, aes(x - 1.5, y - 1.5)) +
                                geom_text_s(aes(label = text),
                                            vjust = "outward_mean",
                                            hjust = "outward_mean")
  )
  vdiffr::expect_doppelganger("geom_text_s19",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text),
                                            vjust = "outward_median",
                                            hjust = "outward_median")
  )
  vdiffr::expect_doppelganger("geom_text_s20",
                              ggplot(df, aes(x - 1.5, y - 1.5)) +
                                geom_text_s(aes(label = text),
                                            vjust = "outward_median",
                                            hjust = "outward_median")
  )
  vdiffr::expect_doppelganger("geom_text_s21",
                              ggplot(df, aes(x - 1.5, y - 1.5)) +
                                geom_text_s(aes(label = text),
                                            vjust = "position",
                                            hjust = "position")
  )
  vdiffr::expect_doppelganger("geom_text_s22",
                              ggplot(df, aes(x - 1.5, y - 1.5)) +
                                geom_text_s(aes(label = text),
                                            vjust = "position",
                                            hjust = "position",
                                            nudge_x = 0.01,
                                            nudge_y = 0.01) +
                                expand_limits(x = 0.6)
  )
  vdiffr::expect_doppelganger("geom_text_s23",
                              ggplot(df, aes(x - 1.5, y - 1.5)) +
                                geom_text_s(aes(label = text),
                                            vjust = "position",
                                            hjust = "position",
                                            nudge_x = -0.01,
                                            nudge_y = -0.01) +
                                expand_limits(x = -0.6)
  )
  vdiffr::expect_doppelganger("geom_text_s24",
                              ggplot(df, aes(x - 1.5, y - 1.5)) +
                                geom_text_s(aes(label = text),
                                            vjust = "position",
                                            hjust = "position",
                                            nudge_x = 0.01,
                                            nudge_y = -0.01) +
                                expand_limits(x = 0.6)
  )
  vdiffr::expect_doppelganger("geom_text_s25",
                              ggplot(df, aes(x - 1.5, y - 1.5)) +
                                geom_text_s(aes(label = text),
                                            vjust = "position",
                                            hjust = "position",
                                            nudge_x = -0.01,
                                            nudge_y = 0.01) +
                                expand_limits(x = -0.6)
  )
  vdiffr::expect_doppelganger("geom_text_s26",
                              ggplot(df, aes(x - 1.5, y - 1.5)) +
                                geom_text_s(aes(label = text),
                                            vjust = "middle",
                                            hjust = "center")  +
                                expand_limits(x = c(-0.6, 0.6))
  )
  vdiffr::expect_doppelganger("geom_text_s27",
                              ggplot(df, aes(x - 1.5, y - 1.5)) +
                                geom_text_s(aes(label = text),
                                            vjust = "top",
                                            hjust = "right")  +
                                expand_limits(x = c(-0.6, 0.6))
  )
  vdiffr::expect_doppelganger("geom_text_s28",
                              ggplot(df, aes(x - 1.5, y - 1.5)) +
                                geom_text_s(aes(label = text),
                                            vjust = "bottom",
                                            hjust = "left")  +
                                expand_limits(x = c(-0.6, 0.6))
  )

})

test_that("text_linked_size", {
# size affects only the text
  my.cars <- datasets::mtcars[c(TRUE, FALSE, FALSE, FALSE), ]
  my.cars$name <- rownames(my.cars)

  vdiffr::expect_doppelganger("geom_text_s30",
                              ggplot(my.cars, aes(wt, mpg, label = name)) +
                                geom_point() +
                                geom_text_s(aes(size = wt), nudge_x = -0.1, hjust = "right") +
                                scale_radius(range = c(3,6)) + # override scale_area()
                                expand_limits(x = c(1.8, 5.5))
  )
}
)

test_that("text_linked_linewidth", {
# segment.linewidth is a parameter, not an aesthetic
  my.cars <- datasets::mtcars[c(TRUE, FALSE, FALSE, FALSE), ]
  my.cars$name <- rownames(my.cars)

  vdiffr::expect_doppelganger("geom_text_s31",
                              ggplot(my.cars, aes(wt, mpg, label = name)) +
                                geom_point() +
                                geom_text_s(segment.linewidth = 1, nudge_x = -0.1, hjust = "right") +
                                scale_radius(range = c(3,6)) + # override scale_area()
                                expand_limits(x = c(1.8, 5.5))
  )
  vdiffr::expect_doppelganger("geom_text_s32",
                              ggplot(my.cars, aes(wt, mpg, label = name)) +
                                geom_point() +
                                geom_text_s(segment.linewidth = 0, nudge_x = -0.1, hjust = "right") +
                                scale_radius(range = c(3,6)) + # override scale_area()
                                expand_limits(x = c(1.8, 5.5))
  )
  vdiffr::expect_doppelganger("geom_text_s33",
                              ggplot(my.cars, aes(wt, mpg, label = name)) +
                                geom_point() +
                                geom_text_s(add.segments = FALSE, nudge_x = -0.1, hjust = "right") +
                                scale_radius(range = c(3,6)) + # override scale_area()
                                expand_limits(x = c(1.8, 5.5))
  )
}
)

test_that("text_linked_color", {

  my.cars <- datasets::mtcars[c(TRUE, FALSE, FALSE, FALSE), ]
  my.cars$name <- rownames(my.cars)

  vdiffr::expect_doppelganger("geom_text_s40",
                              ggplot(my.cars, aes(wt, mpg, label = name)) +
                                geom_text_s(aes(colour = wt), nudge_x = -0.1, hjust = "right") +
                                geom_point() +
                                expand_limits(x = c(1.8, 5.5))
  )
  vdiffr::expect_doppelganger("geom_text_s41",
                              ggplot(my.cars, aes(wt, mpg, label = name)) +
                                geom_text_s(aes(colour = wt), colour.target = "segment",
                                            nudge_x = -0.1, hjust = "right") +
                                geom_point() +
                                expand_limits(x = c(1.8, 5.5))
  )
  vdiffr::expect_doppelganger("geom_text_s42",
                              ggplot(my.cars, aes(wt, mpg, label = name)) +
                                geom_text_s(aes(colour = wt), colour.target = "text",
                                            nudge_x = -0.1, hjust = "right") +
                                geom_point() +
                                expand_limits(x = c(1.8, 5.5))
  )
  vdiffr::expect_doppelganger("geom_text_s43",
                              ggplot(my.cars, aes(wt, mpg, label = name)) +
                                geom_text_s(aes(colour = wt), colour.target = "all",
                                            nudge_x = -0.1, hjust = "right") +
                                geom_point() +
                                expand_limits(x = c(1.8, 5.5))
  )
  vdiffr::expect_doppelganger("geom_text_s45",
                              ggplot(my.cars, aes(wt, mpg, label = name)) +
                                geom_text_s(colour = "red", nudge_x = -0.1, hjust = "right") +
                                geom_point() +
                                expand_limits(x = c(1.8, 5.5))
  )
  vdiffr::expect_doppelganger("geom_text_s46",
                              ggplot(my.cars, aes(wt, mpg, label = name)) +
                                geom_text_s(colour = "red", colour.target = "segment",
                                            nudge_x = -0.1, hjust = "right") +
                                geom_point() +
                                expand_limits(x = c(1.8, 5.5))
  )
  vdiffr::expect_doppelganger("geom_text_s47",
                              ggplot(my.cars, aes(wt, mpg, label = name)) +
                                geom_text_s(colour = "red", colour.target = "text",
                                            nudge_x = -0.1, hjust = "right") +
                                geom_point() +
                                expand_limits(x = c(1.8, 5.5))
  )
  vdiffr::expect_doppelganger("geom_text_s48",
                              ggplot(my.cars, aes(wt, mpg, label = name)) +
                                geom_text_s(colour = "red", colour.target = "all",
                                            nudge_x = -0.1, hjust = "right") +
                                geom_point() +
                                expand_limits(x = c(1.8, 5.5))
  )
  vdiffr::expect_doppelganger("geom_text_s49",
                              ggplot(my.cars, aes(wt, mpg, label = name)) +
                                geom_text_s(colour = "red", colour.target = "text",
                                            default.colour = "blue",
                                            nudge_x = -0.1, hjust = "right") +
                                geom_point(colour = "blue") +
                                expand_limits(x = c(1.8, 5.5))
  )
  vdiffr::expect_doppelganger("geom_text_s50",
                              ggplot(my.cars, aes(wt, mpg, label = name)) +
                                geom_text_s(colour = "red", colour.target = "segment",
                                            default.colour = "blue",
                                            nudge_x = -0.1, hjust = "right") +
                                geom_point(colour = "blue") +
                                expand_limits(x = c(1.8, 5.5))
  )
  vdiffr::expect_doppelganger("geom_text_s51",
                              ggplot(my.cars, aes(wt, mpg, label = name)) +
                                geom_text_s(aes(colour = wt), colour.target = "text",
                                            default.colour = "blue",
                                            nudge_x = -0.1, hjust = "right") +
                                geom_point(colour = "blue") +
                                expand_limits(x = c(1.8, 5.5))
  )
  vdiffr::expect_doppelganger("geom_text_s52",
                              ggplot(my.cars, aes(wt, mpg, label = name)) +
                                geom_text_s(aes(colour = wt), colour.target = "segment",
                                            default.colour = "blue",
                                            nudge_x = -0.1, hjust = "right") +
                                geom_point(colour = "blue") +
                                expand_limits(x = c(1.8, 5.5))
  )
}
)

