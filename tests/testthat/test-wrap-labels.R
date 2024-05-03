context("wrap_labels")

test_that("returns always a character vector", {

  expect_is(wrap_labels(" abcd fghij", width = 5), "character")
  expect_is(wrap_labels(NA, width = 5), "character")
  expect_is(wrap_labels(TRUE, width = 5), "character")
  expect_is(wrap_labels(c(123, 1e6), width = 5), "character")
  expect_is(wrap_labels(c(123, NA), width = 5), "character")
  expect_is(wrap_labels(c(123, 1e6), width = 5), "character")
  expect_is(wrap_labels(character(), width = 5), "character")

})

test_that("white space is consumed or replaced", {

expect_equal(wrap_labels("", width = 20), "")
expect_equal(wrap_labels("\n", width = 20), "")
expect_equal(wrap_labels("\t", width = 20), "")
expect_equal(wrap_labels("   ", width = 20), "")
expect_equal(wrap_labels(" abc ", width = 20), "abc")
expect_equal(wrap_labels(" ab cd ", width = 20), "ab cd")
expect_equal(wrap_labels(" ab\ncd ", width = 20), "ab cd")
expect_equal(wrap_labels(" ab\tcd ", width = 20), "ab cd")

})

test_that("text is wrapped and indented", {
  my.text <- c(A = "This is the first string",
               B = "This is the second string, which is longer")

  expect_length(wrap_labels(my.text, width = 10), 2)
  expect_equal(wrap_labels(my.text[["A"]], width = 20),
               "This is the first\nstring")
  expect_named(wrap_labels(my.text, width = 20), c("A", "B"))
  expect_named(wrap_labels(unname(my.text), width = 20), NULL)
  expect_equal(wrap_labels(my.text[["A"]], width = 20, indent = 2),
               "  This is the first\nstring")
  expect_equal(wrap_labels(my.text[["A"]], width = 20, indent = -2),
               "This is the first\n  string")
})

test_that("test width", {
  my.text.line <- "This is a rather long string that needs wrapping"

  expect_equal(wrap_labels(my.text.line, width = 50), my.text.line)
  expect_equal(wrap_labels(my.text.line, width = 30),
               "This is a rather long string\nthat needs wrapping")
  expect_equal(wrap_labels(my.text.line, width = 20),
               "This is a rather\nlong string that\nneeds wrapping")
  expect_equal(wrap_labels(my.text.line, width = 10),
               "This is a\nrather\nlong\nstring\nthat\nneeds\nwrapping")
  expect_equal(wrap_labels(my.text.line, width = 5),
               "This\nis a\nrather\nlong\nstring\nthat\nneeds\nwrapping")
  # split into words at white space!
  expect_equal(wrap_labels(my.text.line, width = 0),
               "This\nis\na\nrather\nlong\nstring\nthat\nneeds\nwrapping")
  # surprising behaviour of strwrap() retained
  expect_equal(wrap_labels(my.text.line, width = -10),
               wrap_labels(my.text.line, width = 0),)

})

