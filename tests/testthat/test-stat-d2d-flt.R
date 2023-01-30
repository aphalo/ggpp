context("stat_dens2d_filter")

library(ggplot2)
library(tibble)

make_data_tbl <- function(nrow = 100, rfun = rnorm, ...) {
  if (nrow %% 2) {
    nrow <- nrow + 1
  }

  set.seed(1001)

  tibble::tibble(
    x = rfun(nrow, ...),
    y = rfun(nrow, ...),
    group = rep(c("A", "B"), c(nrow / 2, nrow / 2))
  )
}

test_that("filter_params", {
  # Here I have included expect_error() and expect_no_error() tests
  # Many of the "no error" tests complement the vdiffr tests further down

  testthat::expect_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter(keep.fraction = NA)
  )

  testthat::expect_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter(keep.fraction = 5)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter(keep.fraction = 1)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter(keep.fraction = 0)
  )

  testthat::expect_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter(keep.fraction = -1)
  )

  testthat::expect_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter(keep.number = NA)
  )

  testthat::expect_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter(keep.number = -1)
  )

  testthat::expect_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(keep.fraction = NA)
  )

  testthat::expect_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(keep.fraction = 5)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(keep.fraction = 1)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(keep.fraction = 0)
  )

  testthat::expect_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(keep.fraction = -1)
  )

  testthat::expect_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(keep.number = NA)
  )

  testthat::expect_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(keep.number = -1)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(keep.these = 1:3)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(keep.these = TRUE)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(keep.these = FALSE)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(keep.these = rep(c(TRUE, FALSE), 10L))
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(keep.these = function(x) {grepl("^a", x)})
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(keep.these = integer())
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(keep.these = double())
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(keep.these = character())
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(pool.along = "none")
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(pool.along = "none",
                           xintercept = Inf,
                           yintercept = Inf)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(pool.along = "none",
                           xintercept = -Inf,
                           yintercept = -Inf)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(pool.along = "none",
                           xintercept = -Inf,
                           yintercept = Inf)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(pool.along = "none",
                           xintercept = Inf,
                           yintercept = -Inf)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(pool.along = "none",
                           xintercept = Inf)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(pool.along = "none",
                           xintercept = -Inf)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(pool.along = "none",
                           yintercept = Inf)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(pool.along = "none",
                           yintercept = -Inf)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(pool.along = "none",
                           xintercept = -Inf,
                           yintercept = Inf)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(pool.along = "none",
                           xintercept = Inf,
                           yintercept = -Inf)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(pool.along = "none",
                           xintercept = Inf)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(pool.along = "none",
                           xintercept = -Inf)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(pool.along = "none",
                           yintercept = Inf)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(pool.along = "none",
                           yintercept = -Inf)
  )


  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(pool.along = "none",
                           xintercept = -Inf,
                           yintercept = Inf)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(pool.along = "none",
                           xintercept = Inf,
                           yintercept = -Inf)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(pool.along = "y",
                           xintercept = Inf)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(pool.along = "y",
                           xintercept = -Inf)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(pool.along = "x",
                           yintercept = Inf)
  )

  testthat::expect_no_error(
    ggplot(data = make_data_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_filter_g(pool.along = "x",
                           yintercept = -Inf)
  )

})

test_that("numbers_tb", {
  vdiffr::expect_doppelganger("stat_d2d_fltg_01",
                              ggplot(data = make_data_tbl(6), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter_g(colour = "red")
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_01",
                              ggplot(data = make_data_tbl(6), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(colour = "red")
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_02",
                              ggplot(data = make_data_tbl(6), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(colour = "red",
                                                   keep.fraction = 1/2)
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_03",
                              ggplot(data = make_data_tbl(20), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(colour = "red")
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_04",
                              ggplot(data = make_data_tbl(100), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(colour = "red")
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_05",
                              ggplot(data = make_data_tbl(500), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(colour = "red")
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_06",
                              ggplot(data = make_data_tbl(2000), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(colour = "red")
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_07",
                              ggplot(data = make_data_tbl(2000), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(colour = "red",
                                                   keep.fraction = 0.01)
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_08",
                              ggplot(data = make_data_tbl(2000), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(colour = "red",
                                                   keep.sparse = FALSE)
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_09",
                              ggplot(data = make_data_tbl(2000), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(colour = "red",
                                                   keep.sparse = FALSE)+
                                stat_dens2d_filter(colour = "blue")
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_08s",
                              ggplot(data = make_data_tbl(2000), aes(x, y)) +
                                geom_point() +
                                stat_dens1d_filter(colour = "red",
                                                   invert.selection = TRUE)
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_09s",
                              ggplot(data = make_data_tbl(2000), aes(x, y)) +
                                geom_point() +
                                stat_dens1d_filter(colour = "red",
                                                   invert.selection = TRUE)+
                                stat_dens1d_filter(colour = "blue")
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_10",
                              ggplot(data = make_data_tbl(2000), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(colour = "red",
                                                   keep.fraction = 0.01,
                                                   keep.sparse = FALSE)
  )
  # vdiffr::expect_doppelganger("stat_d2d_flt_11",
  #                             ggplot(data = make_data_tbl(10000), aes(x, y)) +
  #                               geom_point() +
  #                               stat_dens2d_filter(colour = "red")
  # )
  # vdiffr::expect_doppelganger("stat_d2d_flt_12",
  #                             ggplot(data = make_data_tbl(10000), aes(x, y)) +
  #                               geom_point() +
  #                               stat_dens2d_filter(colour = "red", keep.fraction = 0.01)
  # )
  vdiffr::expect_doppelganger("stat_d2d_flt_13",
                              ggplot(data = make_data_tbl(1000, rfun = runif), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(colour = "red", keep.fraction = 0.1)
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_14",
                              ggplot(data = make_data_tbl(1000, rfun = rgamma, shape = 2), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(colour = "red", keep.fraction = 0.1)
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_15",
                              ggplot(data = make_data_tbl(1000, rfun = rgamma, shape = 6), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(colour = "red", keep.fraction = 0.1)
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_16",
                              ggplot(data = make_data_tbl(1000, rfun = rbeta, shape1 = 3, shape2 = 12), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(colour = "red", keep.fraction = 0.1)
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_17",
                              ggplot(data = make_data_tbl(1000, rfun = rbeta, shape1 = 3, shape2 = 12), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(colour = "red", keep.fraction = 0.1) +
                                scale_y_log10()
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_18",
                              ggplot(data = make_data_tbl(1000, rfun = rbeta, shape1 = 3, shape2 = 12), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(colour = "red", keep.fraction = 0.1, return.density = TRUE)
  )
})

make_labs_tbl <- function(nrow = 100, rfun = rnorm, ...) {
  if (nrow %% 2) {
    nrow <- nrow + 1
  }

  random_string <-
    function(len = 6) {
      paste(sample(letters, len, replace = TRUE), collapse = "")
    }

  set.seed(1001)

  tibble::tibble(
    x = rfun(nrow, ...),
    y = rfun(nrow, ...),
    group = rep(c("A", "B"), c(nrow / 2, nrow / 2)),
    lab = replicate(nrow, { random_string() })
  )
}

test_that("labels_params", {

  testthat::expect_error(
    ggplot( data = make_labs_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_labels(label.fill = rep("", 2))
  )

  testthat::expect_error(
    ggplot(data = make_labs_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_labels(keep.fraction = NA)
  )

  testthat::expect_error(
    ggplot(data = make_labs_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_labels(keep.fraction = 5)
  )

  testthat::expect_error(
    ggplot(data = make_labs_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_labels(keep.fraction = -1)
  )

  testthat::expect_no_error(
    ggplot(data = make_labs_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_labels(keep.fraction = 0)
  )

  testthat::expect_no_error(
    ggplot(data = make_labs_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_labels(keep.fraction = 1)
  )

  testthat::expect_no_error(
    ggplot(data = make_labs_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_labels(keep.fraction = rep(0.1, 2))
  )

  testthat::expect_no_error(
    ggplot(data = make_labs_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_labels(keep.fraction = rep(0.1, 2),
                         pool.along = "none")
  )

  testthat::expect_no_error(
    ggplot(data = make_labs_tbl(160), aes(x, y, label = lab)) +
      stat_dens2d_labels(keep.fraction = rep(0.1, 4),
                         pool.along = "none")
  )

  testthat::expect_no_error(
    ggplot(data = make_labs_tbl(160), aes(x, y, label = lab)) +
      stat_dens2d_labels(keep.fraction = rep(0.1, 6),
                         pool.along = "none")
  )

  testthat::expect_no_error(
    ggplot(data = make_labs_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_labels(keep.fraction = rep(0.1, 2),
                         pool.along = "x")
  )

  testthat::expect_no_error(
    ggplot(data = make_labs_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_labels(keep.fraction = rep(0.1, 2),
                         pool.along = "y")
  )

  testthat::expect_no_error(
    ggplot(data = make_labs_tbl(100), aes(x, y, label = lab)) +
      stat_dens2d_labels(keep.fraction = 0.1,
                         pool.along = "xy")
  )

  testthat::expect_no_error(
    ggplot(data = make_labs_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_labels(keep.fraction = rep(0.1, 5))
  )

  testthat::expect_error(
    ggplot(data = make_labs_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_labels(keep.number = NA)
  )

  testthat::expect_error(
    ggplot(data = make_labs_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_labels(keep.number = -1)
  )

  testthat::expect_error(
    ggplot(data = make_labs_tbl(20), aes(x, y, label = lab)) +
      stat_dens2d_labels(label.fill = 123)
  )

  testthat::expect_no_error(
    ggplot(data = make_labs_tbl(20), aes(x, y)) +
      stat_dens2d_labels()
  )

  vdiffr::expect_doppelganger("stat_d2d_lbl_fill_x01",
                              ggplot(data = make_labs_tbl(20), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red", label.fill = "")
  )

  vdiffr::expect_doppelganger("stat_d2d_lbl_fill_x02",
                              ggplot(data = make_labs_tbl(20), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red", label.fill = NA)
  )

  vdiffr::expect_doppelganger("stat_d2d_lbl_fill_x03",
                              ggplot(data = make_labs_tbl(20), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red", label.fill = "z")
  )

  vdiffr::expect_doppelganger("stat_d2d_lbl_fill_x04",
                              ggplot(data = make_labs_tbl(20), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red", label.fill = toupper)
  )

  vdiffr::expect_doppelganger("stat_d2d_lbl_fill_x05",
                              ggplot(data = make_labs_tbl(20), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(label.fill = FALSE)
  )

  vdiffr::expect_doppelganger("stat_d2d_lbl_keep_frac_x01",
                              ggplot(data = make_labs_tbl(18), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red", keep.fraction = 1/3)
  )

  vdiffr::expect_doppelganger("stat_d2d_lbl_keep_frac_x02",
                              ggplot(data = make_labs_tbl(18), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red",
                                                   keep.fraction = 1/3,
                                                   pool.along = "none")
  )

  vdiffr::expect_doppelganger("stat_d2d_lbl_keep_frac_x03",
                              ggplot(data = make_labs_tbl(18), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red",
                                                   keep.fraction = 1/3,
                                                   pool.along = "none",
                                                   xintercept = -1)
  )

  vdiffr::expect_doppelganger("stat_d2d_lbl_keep_frac_x04",
                              ggplot(data = make_labs_tbl(18), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red",
                                                   keep.fraction = 1/3,
                                                   pool.along = "none",
                                                   xintercept = 1)
  )

  vdiffr::expect_doppelganger("stat_d2d_lbl_keep_frac_x05",
                              ggplot(data = make_labs_tbl(18), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red",
                                                   keep.fraction = 1/3,
                                                   pool.along = "none",
                                                   xintercept = -3)
  )

  vdiffr::expect_doppelganger("stat_d2d_lbl_keep_frac_x06",
                              ggplot(data = make_labs_tbl(18), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red",
                                                   keep.fraction = 1/3,
                                                   pool.along = "none",
                                                   xintercept = 3)
  )

  vdiffr::expect_doppelganger("stat_d2d_lbl_keep_frac_x07",
                              ggplot(data = make_labs_tbl(18), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red",
                                                   keep.fraction = 1,
                                                   keep.number = 6)
  )

  vdiffr::expect_doppelganger("stat_d2d_lbl_keep_frac_x08",
                              ggplot(data = make_labs_tbl(18), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red",
                                                   keep.fraction = 1,
                                                   keep.number = 12,
                                                   pool.along = "none")
  )

  vdiffr::expect_doppelganger("stat_d2d_lbl_keep_frac_x09",
                              ggplot(data = make_labs_tbl(18), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red",
                                                   keep.fraction = 1,
                                                   keep.number = c(1, 5),
                                                   pool.along = "none")
  )

  vdiffr::expect_doppelganger("stat_d2d_lbl_keep_frac_x10",
                              ggplot(data = make_labs_tbl(18), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red",
                                                   keep.fraction = 1,
                                                   keep.number = c(5, 1),
                                                   pool.along = "none")
  )

  vdiffr::expect_doppelganger("stat_d2d_lbl_keep_frac_x11",
                              ggplot(data = make_labs_tbl(18), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red",
                                                   keep.fraction = c(1/3, 0),
                                                   pool.along = "none")
  )

  vdiffr::expect_doppelganger("stat_d2d_lbl_keep_frac_x12",
                              ggplot(data = make_labs_tbl(18), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red",
                                                   keep.fraction = c(0, 1/3),
                                                   pool.along = "none")
  )
})

test_that("labels_tb", {
  vdiffr::expect_doppelganger("stat_d2d_lbl_x01",
                              ggplot(data = make_labs_tbl(6), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red", keep.fraction = 0)
  )
  vdiffr::expect_doppelganger("stat_d2d_lbl_x02",
                              ggplot(data = make_labs_tbl(6), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red",
                                                   keep.fraction = 1/2)
  )
  vdiffr::expect_doppelganger("stat_d2d_lbl_x03",
                              ggplot(data = make_labs_tbl(20), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red")
  )
  vdiffr::expect_doppelganger("stat_d2d_lbl_x04",
                              ggplot(data = make_labs_tbl(100), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red")
  )
  vdiffr::expect_doppelganger("stat_d2d_lbl_x05",
                              ggplot(data = make_labs_tbl(500), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red")
  )
  vdiffr::expect_doppelganger("stat_d2d_lbl_x06",
                              ggplot(data = make_labs_tbl(2000), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red")
  )
  vdiffr::expect_doppelganger("stat_d2d_lbl_x07",
                              ggplot(data = make_labs_tbl(2000), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red",
                                                   keep.fraction = 0.01)
  )
  vdiffr::expect_doppelganger("stat_d2d_lbl_x08",
                              ggplot(data = make_labs_tbl(2000), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red",
                                                   keep.sparse = FALSE)
  )
  vdiffr::expect_doppelganger("stat_d2d_lbl_x09",
                              ggplot(data = make_labs_tbl(2000), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red",
                                                   keep.sparse = FALSE) +
                                stat_dens2d_labels(colour = "blue")
  )
  vdiffr::expect_doppelganger("stat_d2d_lbl_x10",
                              ggplot(data = make_labs_tbl(2000), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red",
                                                   keep.fraction = 0.01,
                                                   keep.sparse = FALSE)
  )
  # vdiffr::expect_doppelganger("stat_d2d_lbl_x11",
  #                             ggplot(data = make_labs_tbl(10000), aes(x, y, label = lab)) +
  #                               geom_point() +
  #                               stat_dens2d_labels(colour = "red")
  # )
  # vdiffr::expect_doppelganger("stat_d2d_lbl_x12",
  #                             ggplot(data = make_labs_tbl(10000), aes(x, y, label = lab)) +
  #                               geom_point() +
  #                               stat_dens2d_labels(colour = "red",
  #                                                  keep.fraction = 0.01)
  # )
  vdiffr::expect_doppelganger("stat_d2d_lbl_x13",
                              ggplot(data = make_labs_tbl(1000, rfun = runif), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red", keep.fraction = 0.1)
  )
  vdiffr::expect_doppelganger("stat_d2d_lbl_x14",
                              ggplot(data = make_labs_tbl(1000, rfun = rgamma, shape = 2), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red", keep.fraction = 0.1)
  )
  vdiffr::expect_doppelganger("stat_d2d_lbl_x15",
                              ggplot(data = make_labs_tbl(1000, rfun = rgamma, shape = 6), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red", keep.fraction = 0.1)
  )
  vdiffr::expect_doppelganger("stat_d2d_lbl_x16",
                              ggplot(data = make_labs_tbl(1000, rfun = rbeta, shape1 = 3, shape2 = 12), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red", keep.fraction = 0.1)
  )
  vdiffr::expect_doppelganger("stat_d2d_lbl_x17",
                              ggplot(data = make_labs_tbl(1000, rfun = rbeta, shape1 = 3, shape2 = 12), aes(x, y, label = lab)) +
                                geom_point() +
                                stat_dens2d_labels(colour = "red", keep.fraction = 0.1) +
                                scale_x_log10()
  )
})
