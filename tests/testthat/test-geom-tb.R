context("geom_tb")

library(ggplot2)
library(tibble)

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

test_that("multiple_rows_tb", {
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

test_that("numbers_tb", {
  my_data.tb <- tibble(x = -5:5, y = -5:5)
  tb <- tibble(a = 2:4, b = 4:2)
  my.tb <- tibble(x = 0, y = 0, tb = list(tb))
  vdiffr::expect_doppelganger("geom_table_num1",
                              ggplot() +
                                geom_table(data = my.tb,
                                           mapping = aes(x, y, label = tb))
                              )
  vdiffr::expect_doppelganger("geom_table_num2",
                              ggplot(data = my.tb) +
                                geom_table(mapping = aes(x, y, label = tb))
                              )
  vdiffr::expect_doppelganger("geom_table_num3",
                              ggplot() +
                                geom_table(data = my.tb,
                                           mapping = aes(x, y, label = tb),
                                           vjust = 1)
                              )
  vdiffr::expect_doppelganger("geom_table_num4",
                              ggplot(data = my.tb) +
                                geom_table(vjust = 1, hjust = 0,
                                     mapping = aes(x, y, label = tb))
                              )
  vdiffr::expect_doppelganger("geom_table_num5",
                              ggplot(data = my.tb) +
                                geom_table(vjust = 0, hjust = 1,
                                     mapping = aes(x, y, label = tb))
                              )
  vdiffr::expect_doppelganger("geom_table_num6",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb,
                                           mapping = aes(x, y, label = tb))
                              )
})

test_that("theme_tb", {
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

test_that("letters_tb", {
  tb <- tibble(a = LETTERS[2:4], b = letters[4:2])
  my.tb <- tibble(x = 0, y = 0, tb = list(tb))
  vdiffr::expect_doppelganger("geom_table_letters",
                              ggplot() +
                                geom_table(data = my.tb,
                                           mapping = aes(x, y, label = tb))
                              )
})

test_that("parsed_tb", {
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

test_that("geom_table_pos_or_nudge", {
  tb <- tibble(a = 2:4, b = 4:2)
  my.tb <- tibble(x = 0, y = 0, tb = list(tb))
  expect_error(geom_table(data = my.tb,
                          mapping = aes(x, y, label = tb),
                          position = "stack",
                          nudge_x = 0.5,
                          nudge_y = 0.5),
               "You must specify either `position` or `nudge_x`/`nudge_y`.")
})

test_that("geom_table_string_left_hjust", {
  tb <- tibble(a = 2:4, b = 4:2)
  my.tb <- tibble(x = 0, y = 0, tb = list(tb))
x <- geom_table(data = my.tb,
                mapping = aes(x, y, label = tb),
                table.hjust = "left")
expect_equal(x$geom_params$table.hjust, 0)
})

test_that("geom_table_npc_string_center_hjust", {
  tb <- tibble(a = 2:4, b = 4:2)
  my.tb <- tibble(x = 0, y = 0, tb = list(tb))
  p <- ggplot() +
    geom_table_npc(data = my.tb,
                   table.hjust = "center",
                   mapping = aes(npcx = x, npcy = y, label = tb))
  expect_equal(p$layers[[1]]$geom_params$table.hjust, 0.5)

  result <- layer_data(p)[, c("npcx", "npcy", "label", "hjust", "vjust")]
  expect_identical(result$label[[1]], tb)
})
