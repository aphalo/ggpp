context("geom_tb")

library(tibble)
library(dplyr)

test_that("character label gives error in geom_table", {
  my.df <- data.frame(x = 1:10, y = 1:10, tb = letters[1:10])
  expect_error(print(ggplot(my.df, aes(x, y, label = tb)) +
                       geom_table()))
})

test_that("numeric label gives error in geom_table", {
  my.df <- data.frame(x = 1:10, y = 1:10, tb = 1:10)
  expect_error(print(ggplot(my.df, aes(x, y, label = tb)) +
                       geom_table()))
})

get_tb <- function() {
  mtcars %>%
    group_by(cyl) %>%
    summarize(wt = mean(wt), mpg = mean(mpg)) %>%
    ungroup() %>%
    mutate(
      wt = sprintf("%.2f", wt),
      mpg = sprintf("%.1f", mpg)
    )
}

test_that("geom_table works as expected", {
  p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
    geom_point()
  tb <- get_tb()

  tbbl <- tibble(x = 5.45, y = 34, tb = list(tb))
  result <- expect_silent(
    p + geom_table(data = tbbl, aes(x = x, y = y, label = tb))
  )
  expect_s3_class(result, "ggplot")

  df <- data.frame(x = 5.45, y = 34, tb = I(list(tb)))
  result <- expect_silent(
    p + geom_table(data = df, aes(x = x, y = y, label = tb))
  )
  expect_s3_class(result, "ggplot")
})

# test_that("data.frame", {
#   my.df <- data.frame(x = 1:10, y = 1:10, tb = letters[1:10])
#   expect_warning(ggplot(my.df, aes(x, y, label = tb)) +
#                    geom_table())
#   vdiffr::expect_doppelganger("geom_table_text_label",
#                               suppressWarnings(
#                                 ggplot(my.df, aes(x, y, label = tb)) +
#                                   geom_table()
#                               )
#   )
# })

test_that("multiple_rows_tb works in geom_table", {
  tb <- tibble(Z1 = LETTERS[2:4], z1 = letters[4:2])
  tbb <- tibble(Z2 = LETTERS[2:4], z2 = letters[4:2])
  tbbb <- tibble(Z3 = LETTERS[2:4], z3 = letters[4:2])
  my.tb <- tibble(x = 2:4, y = 3:5, tb = list(t1 = tb, t2 = tbb, t3 = tbbb))
  vdiffr::expect_doppelganger("geom_table_multi_row",
                              ggplot() +
                                geom_table(data = my.tb,
                                           mapping = aes(x, y, label = tb)) +
                                lims(x = c(0, 6), y = c(0, 6))
                              )
})

test_that("numbers_tb works in geom_table", {
  my_data.tb <- tibble(x = -5:5, y = -5:5)
  tb <- tibble(a = 2:4, b = 4:2)
  my.tb <- tibble(x = 0, y = 0, tb = list(tb))
  vdiffr::expect_doppelganger("geom_table_geom_data",
                              ggplot() +
                                geom_table(data = my.tb,
                                           mapping = aes(x, y, label = tb))
                              )
  vdiffr::expect_doppelganger("geom_table_plot_data",
                              ggplot(data = my.tb) +
                                geom_table(mapping = aes(x, y, label = tb))
                              )
  vdiffr::expect_doppelganger("geom_table_vjust",
                              ggplot() +
                                geom_table(data = my.tb,
                                           mapping = aes(x, y, label = tb),
                                           vjust = 1)
                              )
  vdiffr::expect_doppelganger("geom_table_vjust_hjust",
                              ggplot(data = my.tb) +
                                geom_table(vjust = 1, hjust = 0,
                                     mapping = aes(x, y, label = tb))
                              )
  vdiffr::expect_doppelganger("geom_table_with_points",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           mapping = aes(x, y, label = tb))
                              )
  vdiffr::expect_doppelganger("geom_table_nudge_x",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           mapping = aes(x, y, label = tb))
  )
  vdiffr::expect_doppelganger("geom_table_nudge_x_nudge_y",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2, nudge_y = 2,
                                           vjust = 0.5, hjust = 1,
                                           mapping = aes(x, y, label = tb))
  )
})

test_that("alpha targets work in geom_table", {
  my_data.tb <- tibble(x = -5:5, y = -5:5)
  tb <- tibble(a = 2:4, b = 4:2)
  my.tb <- tibble(x = 0, y = 0, tb = list(tb))
  vdiffr::expect_doppelganger("geom_table_alpha_aes",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.5,
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_alpha_all",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.5,
                                           alpha.target = "all",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_alpha_segment",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.25,
                                           alpha.target = "segment",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_alpha_table",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.25,
                                           alpha.target = "table",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_alpha_canvas",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.25,
                                           alpha.target = "table.canvas",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_alpha_rules",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.25,
                                           alpha.target = "table.rules",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_alpha_base",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.25,
                                           alpha.target = "table.text",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_alpha_box",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.25,
                                           alpha.target = "box",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_alpha_none",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.25,
                                           alpha.target = "none",
                                           mapping = aes(x, y, label = tb))
  )

})

test_that("colour targets work in geom_table", {
  my_data.tb <- tibble(x = -5:5, y = -5:5)
  tb <- tibble(a = 2:4, b = 4:2)
  my.tb <- tibble(x = 0, y = 0, tb = list(tb))

  vdiffr::expect_doppelganger("geom_table_colour_na",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           colour = NA,
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_colour_aes",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           colour = "red",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_default_colour",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           default.colour = "blue",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_default_colour_none",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           default.colour = "blue",
                                           colour.target = "none",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_colour_all",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           colour = "red",
                                           colour.target = "all",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_colour_segment",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           colour = "red",
                                           colour.target = "segment",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_colour_table",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           colour = "red",
                                           colour.target = "table",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_colour_rules",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           colour = "red",
                                           colour.target = "table.rules",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_colour_base",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           colour = "red",
                                           colour.target = "table.text",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_colour_box",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           colour = "red",
                                           colour.target = "box",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_colour_none",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           colour = "red",
                                           colour.target = "none",
                                           mapping = aes(x, y, label = tb))
  )

})

test_that("colour targets and alpha targets work together in geom_table", {
  my_data.tb <- tibble(x = -5:5, y = -5:5)
  tb <- tibble(a = 2:4, b = 4:2)
  my.tb <- tibble(x = 0, y = 0, tb = list(tb))
  vdiffr::expect_doppelganger("geom_table_colour_alpha_aes",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           colour = "red",
                                           alpha = 0.25,
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_colour_all_alpha",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.25,
                                           alpha.target = "all",
                                           colour = "red",
                                           colour.target = "all",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_colour_segment_alpha_all",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.25,
                                           alpha.target = "all",
                                           colour = "red",
                                           colour.target = "segment",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_colour_segment_alpha_table",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.25,
                                           alpha.target = "table",
                                           colour = "red",
                                           colour.target = "segment",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_colour_table_alpha_table",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.25,
                                           alpha.target = "table",
                                           colour = "red",
                                           colour.target = "table",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_colour_table_alpha_segment",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.25,
                                           alpha.target = "segment",
                                           colour = "red",
                                           colour.target = "table",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_colour_rules_alpha_table",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.25,
                                           alpha.target = "table",
                                           colour = "red",
                                           colour.target = "table.rules",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_colour_rules_alpha_canvas",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.25,
                                           alpha.target = "table.canvas",
                                           colour = "red",
                                           colour.target = "table.rules",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_default_colour_rules_alpha_canvas",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.25,
                                           default.colour = "blue",
                                           alpha.target = "table.canvas",
                                           colour = "red",
                                           colour.target = "table.rules",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_colour_base_alpha_multiple",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.25,
                                           alpha.target = c("table.rules",
                                                            "table.canvas"),
                                           colour = "red",
                                           colour.target = "table.text",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_colour_box2",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.1,
                                           alpha.target = "none",
                                           colour = "red",
                                           colour.target = "box",
                                           mapping = aes(x, y, label = tb))
  )

})

test_that("fill and alpha work together in geom_table", {
  my_data.tb <- tibble(x = -5:5, y = -5:5)
  tb <- tibble(a = 2:4, b = 4:2)
  my.tb <- tibble(x = 0, y = 0, tb = list(tb))
  vdiffr::expect_doppelganger("geom_table_fill_aes",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           fill = "yellow",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_fill_alpha_all",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.25,
                                           alpha.target = "all",
                                           fill = "yellow",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_fill_alpha_table",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.25,
                                           alpha.target = "table",
                                           fill = "yellow",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_fill_alpha_segment",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.25,
                                           alpha.target = "segment",
                                           fill = "yellow",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_fill_alpha_canvas",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.25,
                                           alpha.target = "table.canvas",
                                           fill = "yellow",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_fill_alpha_multiple",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.25,
                                           alpha.target = c("table.rules",
                                                            "table.canvas"),
                                           fill = "yellow",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_fill_alpha_none",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.1,
                                           alpha.target = "none",
                                           fill = "yellow",
                                           mapping = aes(x, y, label = tb))
  )

  vdiffr::expect_doppelganger("geom_table_fill_alpha_na",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           nudge_x = -2,
                                           hjust = 1,
                                           alpha = 0.1,
                                           alpha.target = "none",
                                           fill = NA,
                                           mapping = aes(x, y, label = tb))
  )

})

test_that("table themes work in geom_table", {
  my_data.tb <- tibble(x = -5:5, y = -5:5)
  tb <- tibble(a = 2^(2:4), b = 2^(4:2))
  my.tb <- tibble(x = 0, y = 0, tb = list(tb))
  ttheme_set(ttheme_gtdark)
  vdiffr::expect_doppelganger("ttheme_set_dark",
                              ggplot() +
                                geom_table(data = my.tb,
                                           mapping = aes(x, y, label = tb))
                              )
  ttheme_set()
  vdiffr::expect_doppelganger("ttheme_gtdefault_just_1",
                              ggplot() +
                                geom_table(data = my.tb,
                                           table.theme = ttheme_gtdefault,
                                           table.hjust = 1,
                                           mapping = aes(x, y, label = tb))
                              )
  vdiffr::expect_doppelganger("ttheme_gtdefault_just_0",
                              ggplot() +
                                geom_table(data = my.tb,
                                           table.theme = ttheme_gtdefault,
                                           table.hjust = 0,
                                           mapping = aes(x, y, label = tb)))
  vdiffr::expect_doppelganger("ttheme_gtdefault_just_05",
                              ggplot() +
                                geom_table(data = my.tb,
                                           table.theme = ttheme_gtdefault,
                                           table.hjust = 0.5,
                                           mapping = aes(x, y, label = tb)))
  vdiffr::expect_doppelganger("ttheme_gtdefault_just_right",
                              ggplot() +
                                geom_table(data = my.tb,
                                           table.theme = ttheme_gtdefault,
                                           table.hjust = "right",
                                           mapping = aes(x, y, label = tb)))
  vdiffr::expect_doppelganger("ttheme_gtdefault_just_middle",
                              ggplot() +
                                geom_table(data = my.tb,
                                           table.theme = ttheme_gtdefault,
                                           table.hjust = "middle",
                                           mapping = aes(x, y, label = tb)))
  vdiffr::expect_doppelganger("ttheme_gtdefault_just_center",
                              ggplot() +
                                geom_table(data = my.tb,
                                           table.theme = ttheme_gtdefault,
                                           table.hjust = "center",
                                           mapping = aes(x, y, label = tb)))
  vdiffr::expect_doppelganger("ttheme_gtdefault_just_left",
                              ggplot() +
                                geom_table(data = my.tb,
                                           table.theme = ttheme_gtdefault,
                                           table.hjust = "left",
                                           mapping = aes(x, y, label = tb)))
  vdiffr::expect_doppelganger("ttheme_gtdefault_rownames",
                              ggplot() +
                                geom_table(data = my.tb,
                                           table.theme = ttheme_gtdefault,
                                           table.rownames = TRUE,
                                           mapping = aes(x, y, label = tb)))
  vdiffr::expect_doppelganger("ttheme_gtdefault_colnames",
                              ggplot() +
                                geom_table(data = my.tb,
                                           table.theme = ttheme_gtdefault,
                                           table.colnames = FALSE,
                                           mapping = aes(x, y, label = tb)))
  vdiffr::expect_doppelganger("ttheme_gtminimal",
                              ggplot() +
                                geom_table(data = my.tb,
                                           table.theme = ttheme_gtminimal,
                                           mapping = aes(x, y, label = tb)))
  vdiffr::expect_doppelganger("ttheme_gtsimple",
                              ggplot() +
                                geom_table(data = my.tb,
                                           table.theme = ttheme_gtsimple,
                                           mapping = aes(x, y, label = tb)))
  vdiffr::expect_doppelganger("ttheme_gtdark",
                              ggplot() +
                                geom_table(data = my.tb,
                                           table.theme = ttheme_gtdark,
                                           mapping = aes(x, y, label = tb)))
  vdiffr::expect_doppelganger("ttheme_gtlight",
                              ggplot() +
                                geom_table(data = my.tb,
                                           table.theme = ttheme_gtlight,
                                           mapping = aes(x, y, label = tb)))
  vdiffr::expect_doppelganger("ttheme_gtstripes",
                              ggplot() +
                                geom_table(data = my.tb,
                                           table.theme = ttheme_gtstripes,
                                           mapping = aes(x, y, label = tb)))
})

test_that("letters_tb works in geom_table", {
  tb <- tibble(a = LETTERS[2:4], b = letters[4:2])
  my.tb <- tibble(x = 0, y = 0, tb = list(tb))
  vdiffr::expect_doppelganger("geom_table_letters",
                              ggplot() +
                                geom_table(data = my.tb,
                                           mapping = aes(x, y, label = tb))
                              )
})

test_that("parsed_tb works in geom_table", {
  tb <- tibble("alpha" = c("x[2]~\"=\"~a^2", "sqrt(y)"),
               "beta" = c("x[2]~\"=\"~b^2", "sqrt(1/y)"))
  my.tb <- tibble(x = 0, y = 0, tb = list(tb))
  vdiffr::expect_doppelganger("geom_table_parsed_all",
                              ggplot() +
                                geom_table(my.tb, mapping = aes(x, y, label = tb), parse = TRUE)
                              )

  tb <- tibble("alpha" = c("x[2]~\"=\"~a^2", "text"),
               "beta" = c("x[2]~\"=\"~b^2", "1200"))
  my.tb <- tibble(x = 0, y = 0, tb = list(tb))
  vdiffr::expect_doppelganger("geom_table_parsed_partial",
                              ggplot() +
                                geom_table(my.tb, mapping = aes(x, y, label = tb), parse = TRUE)
                              )
  vdiffr::expect_doppelganger("geom_table_parsed_partial_dark",
                              ggplot() +
                                geom_table(my.tb,
                                           table.theme = ttheme_gtdark,
                                           mapping = aes(x, y, label = tb),
                                           parse = TRUE)
  )
})

test_that("pos_or_nudge work in geom_table", {
  tb <- tibble(a = 2:4, b = 4:2)
  my.tb <- tibble(x = 0, y = 0, tb = list(tb))
  expect_error(geom_table(data = my.tb,
                          mapping = aes(x, y, label = tb),
                          position = "stack",
                          nudge_x = 0.5,
                          nudge_y = 0.5),
               "You must specify either `position` or `nudge_x`/`nudge_y`.")
})

test_that("string_left_hjustworks in geom_table", {
  tb <- tibble(a = 2:4, b = 4:2)
  my.tb <- tibble(x = 0, y = 0, tb = list(tb))
x <- geom_table(data = my.tb,
                mapping = aes(x, y, label = tb),
                table.hjust = "left")
expect_equal(x$geom_params$table.hjust, 0)
})
