context("ggplot.ts")

test_that("ggplot.ts works as expected with the yearly time series.", {
  data <- lynx

  expect_silent(p <- ggplot.ts(data) + geom_line())
  res <- layer_data(p)

  expect_equal(res$x, as.numeric(time(data)))
  expect_equal(res$y, as.numeric(data))

  # argument to data is not an object name
  expect_silent(p <- ggplot.ts(get("data")) + geom_line())
  res <- layer_data(p)

  expect_equal(res$x, as.numeric(time(data)))
  expect_equal(res$y, as.numeric(data))
})

test_that("ggplot.ts works as expected with the monthly time series.", {
  data <- sunspots

  expect_silent(p <- ggplot.ts(data) + geom_line())
  res <- layer_data(p)

  expect_equal(res$x, as.numeric(time(data)))
  expect_equal(res$y, as.numeric(data))
})

test_that("ggplot.ts works as expected with the quaterly time series.", {
  data <- presidents

  expect_silent(p <- ggplot.ts(data) + geom_line())
  res <- layer_data(p)

  expect_equal(res$x, as.numeric(time(data)), tolerance = 0.001)
  expect_equal(res$y, as.numeric(data))
})

test_that("ggplot.ts time resolution works as expected.", {
  data <- sunspot.month
  expected <- zoo::as.Date(time(data))
  expected <- lubridate::round_date(expected, unit = "year")
  expected_pos <- as.numeric(as.POSIXct(expected))

  expect_silent(
    p <- ggplot.ts(data, as.numeric = FALSE, time.resolution = "year") +
      geom_point()
  )
  res <- layer_data(p, 1)

  expect_equal(res$x, expected_pos)
  expect_equal(res$y, as.numeric(data))
})

test_that("ggplot.ts mapping works as expected.", {
  data <- sunspot.month

  data <- sunspot.month
  expected <- zoo::as.Date(time(data))
  expected <- lubridate::round_date(expected, unit = "10 year")

  expected_med <- as.numeric(tapply(as.numeric(data), expected, median))
  expected_q1 <- as.numeric(tapply(as.numeric(data), expected, quantile, probs = 0.25))
  expected_q3 <- as.numeric(tapply(as.numeric(data), expected, quantile, probs = 0.75))

  expect_silent(
    p <- ggplot.ts(
      data,
      mapping = aes(x = data, y = time, group = time),
      as.numeric = FALSE,
      time.resolution = "10 year"
    ) +
      geom_boxplot()
  )

  res <- layer_data(p)

  expect_equal(res$y, unique(as.numeric(as.POSIXct(expected))))
  expect_equal(res$xmiddle, expected_med)
  expect_equal(res$xlower, expected_q1)
  expect_equal(res$xupper, expected_q3)
})
