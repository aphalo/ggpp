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
