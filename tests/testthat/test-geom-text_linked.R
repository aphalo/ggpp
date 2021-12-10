context("geom_text_linked")

test_that("text_linked_just", {
  df <- data.frame(
    x = c(1, 1, 2, 2, 1.5),
    y = c(1, 2, 1, 2, 1.5),
    text = c("bottom-left", "bottom-right", "top-left", "top-right", "center")
  )

  vdiffr::expect_doppelganger("geom_text_linked1",
                              ggplot(df, aes(x, y)) +
                                geom_text_linked(aes(label = text))
  )

  vdiffr::expect_doppelganger("geom_text_linked2",
                              ggplot(df, aes(x, y)) +
                                geom_text_linked(aes(label = text), vjust = "inward", hjust = "inward")
  )
  vdiffr::expect_doppelganger("geom_text_linked3",
                              ggplot(df, aes(x, y)) +
                                geom_text_linked(aes(label = text), hjust = "inward", angle = 33)
  )
  vdiffr::expect_doppelganger("geom_text_linked4",
                              ggplot(df, aes(x, y)) +
                                geom_text_linked(aes(label = text), hjust = "inward", angle = 66)
  )
  vdiffr::expect_doppelganger("geom_text_linked5",
                              ggplot(df, aes(x, y)) +
                                geom_text_linked(aes(label = text), hjust = "inward", angle = 90)
  )
  vdiffr::expect_doppelganger("geom_text_linked7",
                              ggplot(df, aes(x, y)) +
                                geom_text_linked(aes(label = text), vjust = "inward", hjust = "inward", angle = 33)
  )
  vdiffr::expect_doppelganger("geom_text_linked8",
                              ggplot(df, aes(x, y)) +
                                geom_text_linked(aes(label = text), vjust = "inward", hjust = "inward", angle = 66)
  )
  vdiffr::expect_doppelganger("geom_text_linked9",
                              ggplot(df, aes(x, y)) +
                                geom_text_linked(aes(label = text), vjust = "inward", hjust = "inward", angle = 90)
  )
  vdiffr::expect_doppelganger("geom_text_linked10",
                              ggplot(df, aes(x, y)) +
                                geom_text_linked(aes(label = text), vjust = "inward", hjust = "inward", angle = 90)
  )
  vdiffr::expect_doppelganger("geom_text_linked11",
                              ggplot(df, aes(x, y)) +
                                geom_text_linked(aes(label = text), vjust = "inward_1.5", hjust = "inward_1.5")
  )
  vdiffr::expect_doppelganger("geom_text_linked12",
                              ggplot(df, aes(x - 1.5, y - 1.5)) +
                                geom_text_linked(aes(label = text), vjust = "inward_0.0", hjust = "inward_0.0")
  )
  vdiffr::expect_doppelganger("geom_text_linked13",
                              ggplot(df, aes(x, y)) +
                                geom_text_linked(aes(label = text), vjust = "inward_mean", hjust = "inward_mean")
  )
  vdiffr::expect_doppelganger("geom_text_linked14",
                              ggplot(df, aes(x - 1.5, y - 1.5)) +
                                geom_text_linked(aes(label = text), vjust = "inward_mean", hjust = "inward_mean")
  )
  vdiffr::expect_doppelganger("geom_text_linked15",
                              ggplot(df, aes(x, y)) +
                                geom_text_linked(aes(label = text), vjust = "inward_median", hjust = "inward_median")
  )

  vdiffr::expect_doppelganger("geom_text_linked16",
                              ggplot(df, aes(x, y)) +
                                geom_text_linked(aes(label = text), vjust = "outward", hjust = "outward")
  )
  vdiffr::expect_doppelganger("geom_text_linked17",
                              ggplot(df, aes(x, y)) +
                                geom_text_linked(aes(label = text), vjust = "outward_mean", hjust = "outward_mean")
  )
  vdiffr::expect_doppelganger("geom_text_linked18",
                              ggplot(df, aes(x - 1.5, y - 1.5)) +
                                geom_text_linked(aes(label = text), vjust = "outward_mean", hjust = "outward_mean")
  )
  vdiffr::expect_doppelganger("geom_text_linked19",
                              ggplot(df, aes(x, y)) +
                                geom_text_linked(aes(label = text), vjust = "outward_median", hjust = "outward_median")
  )
  vdiffr::expect_doppelganger("geom_text_linked20",
                              ggplot(df, aes(x - 1.5, y - 1.5)) +
                                geom_text_linked(aes(label = text), vjust = "outward_median", hjust = "outward_median")
  )

})

test_that("text_linked_size", {

  my.cars <- datasets::mtcars[c(TRUE, FALSE, FALSE, FALSE), ]
  my.cars$name <- rownames(my.cars)

  vdiffr::expect_doppelganger("geom_text_linked30",
                              ggplot(my.cars, aes(wt, mpg, label = name)) +
                                geom_point() +
                                geom_text_linked(aes(size = wt), nudge_x = -0.1, hjust = "right") +
                                scale_radius(range = c(3,6)) + # override scale_area()
                                expand_limits(x = c(1.8, 5.5))
  )
}
)
