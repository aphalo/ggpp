context("geom_text_pairwise")

test_that("text_pairwise_default", {
  df <- data.frame(
    x.min = c(1, 3, 5),
    x.max = c(2, 4, 7),
    y = c(2, 4, 3),
    text = c("1-2", "3-4", "5-7")
  )

  vdiffr::expect_doppelganger("geom_text_pairwise_inherit.aes",
                              ggplot() +
                                geom_text_pairwise(data = df,
                                                   mapping = aes(xmin = x.min,
                                                                 xmax = x.max,
                                                                 y = y,
                                                                 label = text))
  )

  vdiffr::expect_doppelganger("geom_text_pairwise_default",
                              ggplot(df,
                                     aes(xmin = x.min,
                                         xmax = x.max,
                                         y = y,
                                         label = text)) +
                                geom_text_pairwise(inherit.aes = TRUE)
  )

  vdiffr::expect_doppelganger("geom_text_pairwise_hjust",
                              ggplot(df,
                                     aes(xmin = x.min,
                                         xmax = x.max,
                                         x = x.min,
                                         y = y,
                                         label = text)) +
                                geom_text_pairwise(inherit.aes = TRUE,
                                                   hjust = 0)
  )

  vdiffr::expect_doppelganger("geom_text_pairwise_below",
                              ggplot(df,
                                     aes(xmin = x.min,
                                         xmax = x.max,
                                         y = y,
                                         label = text)) +
                                geom_text_pairwise(inherit.aes = TRUE,
                                                   vjust = 1.1)
  )

  vdiffr::expect_doppelganger("geom_text_pairwise_arrow",
                              ggplot(df,
                                     aes(xmin = x.min,
                                         xmax = x.max,
                                         y = y,
                                         label = text)) +
                                geom_text_pairwise(inherit.aes = TRUE,
                                                   arrow = grid::arrow(ends = "both"))
  )

    vdiffr::expect_doppelganger("geom_text_pairwise_angle",
                              ggplot(df,
                                     aes(xmin = x.min,
                                         xmax = x.max,
                                         y = y,
                                         label = text)) +
                                geom_text_pairwise(inherit.aes = TRUE,
                                                   hjust = -0.2,
                                                   vjust = 0.5,
                                                   angle = 90)
  )

})

test_that("text_pairwise_segment", {
# segment.linewidth is a parameter, not an aesthetic
  df <- data.frame(
    x.min = c(1, 3, 5),
    x.max = c(2, 4, 7),
    y = c(2, 4, 3),
    text = c("1-2", "3-4", "5-7")
  )

  vdiffr::expect_doppelganger("geom_text_pairwise_segment.linewidth",
                              ggplot(df,
                                     aes(xmin = x.min,
                                         xmax = x.max,
                                         y = y,
                                         label = text)) +
                                geom_text_pairwise(inherit.aes = TRUE,
                                                   segment.linewidth = 1)
  )

  vdiffr::expect_doppelganger("geom_text_pairwise_colour",
                              ggplot(df,
                                     aes(xmin = x.min,
                                         xmax = x.max,
                                         y = y,
                                         label = text)) +
                                geom_text_pairwise(inherit.aes = TRUE,
                                                   colour = "red")
  )

  vdiffr::expect_doppelganger("geom_text_pairwise_color",
                              ggplot(df,
                                     aes(xmin = x.min,
                                         xmax = x.max,
                                         y = y,
                                         label = text)) +
                                geom_text_pairwise(inherit.aes = TRUE,
                                                   color = "red")
  )

  vdiffr::expect_doppelganger("geom_text_pairwise_color.target.all",
                              ggplot(df,
                                     aes(xmin = x.min,
                                         xmax = x.max,
                                         y = y,
                                         label = text)) +
                                geom_text_pairwise(inherit.aes = TRUE,
                                                   color.target = "all",
                                                   color = "red")
  )

  vdiffr::expect_doppelganger("geom_text_pairwise_color.target.text",
                              ggplot(df,
                                     aes(xmin = x.min,
                                         xmax = x.max,
                                         y = y,
                                         label = text)) +
                                geom_text_pairwise(inherit.aes = TRUE,
                                                   color.target = "text",
                                                   color = "red")
  )

  vdiffr::expect_doppelganger("geom_text_pairwise_color.target.segment",
                              ggplot(df,
                                     aes(xmin = x.min,
                                         xmax = x.max,
                                         y = y,
                                         label = text)) +
                                geom_text_pairwise(inherit.aes = TRUE,
                                                   color.target = "segment",
                                                   color = "red")
  )

}
)

test_that("text_pairwise_position", {
  df <- data.frame(
    x.min = c(1, 3, 5),
    x.max = c(2, 4, 7),
    y = c(2, 4, 3),
    text = c("1-2", "3-4", "5-7")
  )

  vdiffr::expect_doppelganger("geom_text_pairwise_nudge_x",
                              ggplot(df,
                                     aes(xmin = x.min,
                                         xmax = x.max,
                                         y = y,
                                         label = text)) +
                                geom_text_pairwise(inherit.aes = TRUE,
                                                   nudge_x = 0.5)
  )

  vdiffr::expect_doppelganger("geom_text_pairwise_nudge_y",
                              ggplot(df,
                                     aes(xmin = x.min,
                                         xmax = x.max,
                                         y = y,
                                         label = text)) +
                                geom_text_pairwise(inherit.aes = TRUE,
                                                   nudge_y = 0.5)
  )

  expect_error(ggplot(df,
                      aes(xmin = x.min,
                          xmax = x.max,
                          y = y,
                          label = text)) +
                 geom_text_pairwise(inherit.aes = TRUE,
                                    nudge_x = 0.5,
                                    position = position_nudge(x = 0.5, y = 0))
  )

})

