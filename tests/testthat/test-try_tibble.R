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
