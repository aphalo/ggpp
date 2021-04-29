context("stat_peaks")

library(tibble)
library(lubridate)

make_data_tbl <- function(nrow = 100, rfun = rnorm, ...) {
  if (nrow %% 2) {
    nrow <- nrow + 1
  }

  set.seed(101)

  tibble::tibble(
    x = 1:nrow,
    x.date = ydm("2000-01-01") + days(x),
    x.datetime = ydm_hms("2000-01-01 00:00:01") + days(x),
    y = rfun(nrow, ...),
    group = rep(c("A", "B"), nrow / 2)
  )
}

test_that("numbers_tb", {
  vdiffr::expect_doppelganger("stat_peaks_numeric_01",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(colour = "red") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_numeric__02",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(geom = "text", vjust = -0.5) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_numeric__03",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(geom = "text", vjust = -0.5,
                                           x.label.fmt = "x = %.0f") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_numeric__04",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(mapping = aes(label = stat(y.label)),
                                           geom = "text", hjust = -0.1,
                                           y.label.fmt = "lambda~`=`~%.2f",
                                           angle = 90,
                                           parse = TRUE) +
                              expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_numeric__05",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(colour = "red", span = NULL) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_numeric__06",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(colour = "red", span = 11) +
                                expand_limits(y = c(-2.5, 2.5))
  )
})

test_that("dates_tb", {
  vdiffr::expect_doppelganger("stat_peaks_date_01",
                              ggplot(data = make_data_tbl(30), aes(x.date, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(colour = "red") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_date_02",
                              ggplot(data = make_data_tbl(30), aes(x.date, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(geom = "text", vjust = -0.5) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_date_03",
                              ggplot(data = make_data_tbl(30), aes(x.datetime, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(geom = "text", vjust = -0.5,
                                           x.label.fmt = "%b-%d") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_date_04",
                              ggplot(data = make_data_tbl(30), aes(x.datetime, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(geom = "text", vjust = -0.5,
                                           x.label.fmt = "lambda~`=`~\"%b-%d\"",
                                           parse = TRUE) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_date_05",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(mapping = aes(label = stat(y.label)),
                                           geom = "text", hjust = -0.1,
                                           y.label.fmt = "lambda~`=`~%.2f",
                                           angle = 90,
                                           parse = TRUE) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_date_06",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(colour = "red", span = NULL) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_date_07",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(colour = "red", span = 11) +
                                expand_limits(y = c(-2.5, 2.5))
  )
})

test_that("datetimes_tb", {
  vdiffr::expect_doppelganger("stat_peaks_datetime_01",
                              ggplot(data = make_data_tbl(30), aes(x.datetime, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(colour = "red") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_datetime_02",
                              ggplot(data = make_data_tbl(30), aes(x.datetime, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(geom = "text", vjust = -0.5) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_datetime_03",
                              ggplot(data = make_data_tbl(30), aes(x.datetime, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(geom = "text", vjust = -0.5,
                                           x.label.fmt = "%b-%d") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_datetime_04",
                              ggplot(data = make_data_tbl(30), aes(x.datetime, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(geom = "text", vjust = -0.5,
                                           x.label.fmt = "lambda~`=`~\"%b-%d\"",
                                           parse = TRUE) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_datetime_05",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(mapping = aes(label = stat(y.label)),
                                           geom = "text", hjust = -0.1,
                                           y.label.fmt = "lambda~`=`~%.2f",
                                           angle = 90,
                                           parse = TRUE) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_datetime_06",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(colour = "red", span = NULL) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_datetime_07",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(colour = "red", span = 11) +
                                expand_limits(y = c(-2.5, 2.5))
  )
})
