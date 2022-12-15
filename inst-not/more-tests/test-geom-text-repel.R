context("geom_text_repel")

library(ggrepel)

test_that("text_repel_just", {
  df <- data.frame(
    x = c(1, 1, 2, 2, 1.5),
    y = c(1, 2, 1, 2, 1.5),
    text = c("bottom-left", "top-left", "bottom-right", "top-right", "center")
  )

  set.seed(98765)
  vdiffr::expect_doppelganger("geom_text_repel1",
                              ggplot(df, aes(x, y)) +
                                geom_point(colour = "blue") +
                                geom_text_repel(aes(label = text), position = position_nudge_to(y = 1.25),
                                                hjust = "outward")
  )

  vdiffr::expect_doppelganger("geom_text_repel2",
                              ggplot(df, aes(x, y)) +
                                geom_point(colour = "blue") +
                                geom_text_repel(aes(label = text), position = position_nudge_center(x = -0.1, direction = "split"),
                                                hjust = "inward")
  )
  vdiffr::expect_doppelganger("geom_text_repel3",
                              ggplot(df, aes(x, y)) +
                                geom_point(colour = "blue") +
                                geom_text_repel(aes(label = text), position = position_nudge_center(x = 0.1, direction = "split"),
                                                hjust = "outward")
  )
  vdiffr::expect_doppelganger("geom_text_repel4",
                              ggplot(df, aes(x, y)) +
                                geom_point(colour = "blue") +
                                geom_text_repel(aes(label = text), position = position_nudge_center(x = -0.1, y = -0.1, direction = "radial"),
                                                hjust = "outward")
  )
  vdiffr::expect_doppelganger("geom_text_repel5",
                              ggplot(df, aes(x, y)) +
                                geom_point(colour = "blue") +
                                geom_text_repel(aes(label = text), position = position_nudge_keep(x = -0.1, y = -0.1),
                                                hjust = "outward")
  )
  vdiffr::expect_doppelganger("geom_text_repel6",
                              ggplot(df, aes(x, y)) +
                                geom_point(colour = "blue") +
                                geom_text_repel(aes(label = text), position = position_nudge_keep(x = 0.1, y = 0.1))
  )

})

test_that("text_repel_size", {

  my.cars <- datasets::mtcars[c(TRUE, FALSE), ]
  my.cars$name <- rownames(my.cars)

  set.seed(98765)
  vdiffr::expect_doppelganger("geom_text_repel20",
                              ggplot(my.cars, aes(wt, mpg, label = name)) +
                                geom_point() +
                                geom_text_repel(aes(size = wt), nudge_x = -0.1, min.segment.length = 0) +
                                scale_radius(range = c(3,6)) + # override scale_area()
                                expand_limits(x = c(1.8, 5.5))
  )
  set.seed(98765)
  vdiffr::expect_doppelganger("geom_text_repel21",
                              ggplot(my.cars, aes(wt, mpg, label = name)) +
                                geom_point() +
                                geom_text_repel(aes(size = wt),
                                                position = position_nudge_line(x = 0.2, y = 2), min.segment.length = 0) +
                                scale_radius(range = c(3,6)) + # override scale_area()
                                expand_limits(x = c(1.8, 5.5))
  )
}
)

test_that("position_nudge_repel", {

  my.cars <- datasets::mtcars[c(TRUE, FALSE), ]
  my.cars$name <- rownames(my.cars)

  set.seed(98765)
  vdiffr::expect_doppelganger("geom_text_repel30",
                              ggplot(my.cars, aes(wt, mpg, label = name)) +
                                geom_point() +
                                geom_text_s(aes(size = wt),
                                                position = position_nudge_repel(x = 0.2, y = 2), min.segment.length = 0) +
                                scale_radius(range = c(3,6)) + # override scale_area()
                                expand_limits(x = c(1.8, 5.5))
  )
}
)
