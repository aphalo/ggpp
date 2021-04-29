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

test_that("numbers_tb", {
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
