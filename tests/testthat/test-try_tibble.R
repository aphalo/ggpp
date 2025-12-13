context("try_tibble")

lynx

test_that("try_tibble, compare output", {

  tb <- try_tibble(lynx, as.numeric = TRUE)

  expect_equal(length(lynx), nrow(tb))
})

test_that("try_tibble, check class", {

  tb <- try_tibble(lynx, as.numeric = TRUE)

  expect_s3_class(tb, "tbl_df")
})

test_that("try_tibble, check format of time", {

  df <- try_tibble(lynx, "year")
  expect_equal(class(df$time), "Date")

  df <- try_tibble(lynx, as.numeric = TRUE)
  expect_equal(class(df$time), "numeric")
})

test_that("try_tibble, with xts object with hourly data", {

  my.xts <- xts::as.xts(data.frame(a = 1:50,
                                   t = lubridate::now() + lubridate::hours(1:50)))
  df <- try_tibble(my.xts, "year")
  expect_equal(class(df$time)[1], "POSIXct")

  df <- try_tibble(my.xts, as.numeric = TRUE)
  expect_equal(class(df$time), "numeric")
})

test_that("try_tibble, with xts object with daily data", {

  my.xts <- xts::as.xts(data.frame(a = 1:50,
                                   t = lubridate::now() + lubridate::days(1:50)))
  df <- try_tibble(my.xts, "day")
  expect_equal(class(df$time)[1], "POSIXct")

  df <- try_tibble(my.xts, as.numeric = TRUE)
  expect_equal(class(df$time), "numeric")
})

test_that("try_tibble, with xts object with daily data as Dates", {

  my.xts <- xts::as.xts(data.frame(a = 1:50,
                                   t = as.Date(
                                     lubridate::now() + lubridate::days(1:50)
                                   )))
  df <- try_tibble(my.xts, "day")
  expect_equal(class(df$time)[1], "Date")

  df <- try_tibble(my.xts, as.numeric = TRUE)
  expect_equal(class(df$time), "numeric")
})
