context("geom_text_s")

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
                                geom_text_s(aes(label = text), vjust = "inward", hjust = "inward")
  )
  vdiffr::expect_doppelganger("geom_text_s3",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text), hjust = "inward", angle = 33)
  )
  vdiffr::expect_doppelganger("geom_text_s4",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text), hjust = "inward", angle = 66)
  )
  vdiffr::expect_doppelganger("geom_text_s5",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text), hjust = "inward", angle = 90)
  )
  vdiffr::expect_doppelganger("geom_text_s7",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text), vjust = "inward", hjust = "inward", angle = 33)
  )
  vdiffr::expect_doppelganger("geom_text_s8",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text), vjust = "inward", hjust = "inward", angle = 66)
  )
  vdiffr::expect_doppelganger("geom_text_s9",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text), vjust = "inward", hjust = "inward", angle = 90)
  )
  vdiffr::expect_doppelganger("geom_text_s10",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text), vjust = "inward", hjust = "inward", angle = 90)
  )
  vdiffr::expect_doppelganger("geom_text_s11",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text), vjust = "inward_1.5", hjust = "inward_1.5")
  )
  vdiffr::expect_doppelganger("geom_text_s12",
                              ggplot(df, aes(x - 1.5, y - 1.5)) +
                                geom_text_s(aes(label = text), vjust = "inward_0.0", hjust = "inward_0.0")
  )
  vdiffr::expect_doppelganger("geom_text_s13",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text), vjust = "inward_mean", hjust = "inward_mean")
  )
  vdiffr::expect_doppelganger("geom_text_s14",
                              ggplot(df, aes(x - 1.5, y - 1.5)) +
                                geom_text_s(aes(label = text), vjust = "inward_mean", hjust = "inward_mean")
  )
  vdiffr::expect_doppelganger("geom_text_s15",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text), vjust = "inward_median", hjust = "inward_median")
  )

  vdiffr::expect_doppelganger("geom_text_s16",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text), vjust = "outward", hjust = "outward")
  )
  vdiffr::expect_doppelganger("geom_text_s17",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text), vjust = "outward_mean", hjust = "outward_mean")
  )
  vdiffr::expect_doppelganger("geom_text_s18",
                              ggplot(df, aes(x - 1.5, y - 1.5)) +
                                geom_text_s(aes(label = text), vjust = "outward_mean", hjust = "outward_mean")
  )
  vdiffr::expect_doppelganger("geom_text_s19",
                              ggplot(df, aes(x, y)) +
                                geom_text_s(aes(label = text), vjust = "outward_median", hjust = "outward_median")
  )
  vdiffr::expect_doppelganger("geom_text_s20",
                              ggplot(df, aes(x - 1.5, y - 1.5)) +
                                geom_text_s(aes(label = text), vjust = "outward_median", hjust = "outward_median")
  )

})

test_that("text_linked_size", {

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
