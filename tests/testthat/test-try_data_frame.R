context("try_data_frame")

lynx

test_that("try_data_frame, compare output", {

  df <- try_data_frame(lynx, as.numeric = TRUE)

  expect_equal(length(lynx), nrow(df))
})

test_that("try_data_frame, check if dataframe", {

  df <- try_data_frame(lynx, as.numeric = TRUE)

  expect_s3_class(df, "data.frame")
})

test_that("try_data_frame, check format of time", {

  df <- try_data_frame(lynx, "year")
  expect_equal(class(df$time), "Date")

  df <- try_data_frame(lynx, as.numeric = TRUE)
  expect_equal(class(df$time), "numeric")
})

test_that("try_data_frame, check format of time with xts", {
  arg.xts <- xts::as.xts(lynx)
  df <- try_data_frame(arg.xts, "year")
  expect_equal(class(df$time), "Date")

  df <- try_data_frame(lynx, as.numeric = TRUE)
  expect_equal(class(df$time), "numeric")
})

test_that("try_data_frame, if data frame pass through", {

  arg.df <- data.frame(x = 1:10, y = 10:1)
  df <- try_data_frame(arg.df)

  expect_s3_class(df, "data.frame")
  expect_identical(arg.df, df)
})

test_that("try_data_frame, if list to data frame", {

  arg.lst <- list(x = 1:10, y = 10:1)
  df <- try_data_frame(arg.lst)

  expect_s3_class(df, "data.frame")
  expect_equal(as.data.frame(arg.lst), df)
})

test_that("ggplot.ts, if list to data frame num", {

  p <- ggplot(lynx)

  expect_s3_class(p$data, "data.frame")
  expect_named(p$data, c("time", "lynx"))
  expect_is(p$data$time, "numeric")
  expect_is(p$data$lynx, "numeric")
})

test_that("ggplot.ts, if list to data frame date", {

  p <- ggplot(lynx, as.numeric = FALSE)

  expect_s3_class(p$data, "data.frame")
  expect_named(p$data, c("time", "lynx"))
  expect_is(p$data$time, "Date")
  expect_is(p$data$lynx, "numeric")
})



