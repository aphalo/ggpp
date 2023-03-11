test_that("parse_safe works with simple expressions", {
  expect_equal(
    parse_safe(c("", " ", "     ")),
    expression(NA, NA, NA)
  )

  expect_equal(
    parse_safe(c("A", "B", "C")),
    expression(A, B, C)
  )

  expect_equal(
    parse_safe(c("alpha", "", "gamma", " ")),
    expression(alpha, NA, gamma, NA)
  )

  expect_equal(
    parse_safe(c(NA, "a", NA, "alpha")),
    expression(NA, a, NA, alpha)
  )
})

test_that("parse_safe works with multi expressions", {
  expect_equal(
    parse_safe(c(" \n", "\n ", " \n  \n  ")),
    expression(NA, NA, NA)
  )

  expect_equal(
    parse_safe(c("alpha ~ beta", "beta \n gamma", "")),
    expression(alpha ~ beta, beta, NA)
  )

  expect_equal(
    parse_safe(c("alpha ~ beta", " ", "integral(f(x) * dx, a, b)")),
    expression(alpha ~ beta, NA, integral(f(x) * dx, a, b))
  )

  expect_equal(
    parse_safe(c(NA, 1, 2, "a \n b")),
    expression(NA, 1, 2, a)
  )
})

test_that("new_data_frame", {
  expect_is(new_data_frame(), "data.frame")
  df <- new_data_frame(list(A = 1:2, B = letters[1:2]))
  expect_is(df, "data.frame")
  expect_named(df, c("A", "B"))
  expect_equal(nrow(df), 2L)
  expect_error(new_data_frame(list(1:2, letters[1:2])))
  expect_error(new_data_frame(list(A = 1:2, B = letters[1:2]), n = 0))
  expect_error(new_data_frame(list(A = 1:2, B = letters[1:2]), n = 10))
})
