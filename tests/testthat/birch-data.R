context("birch_data")

test_that("birch.df test size and names", {
expect_identical(nrow(birch.df), 350L)
expect_identical(ncol(birch.df), 8L)
expect_named(birch.df,
             c("Container", "Density", "block", "height", "diameter",
               "dwstem", "dwroot", "healthy"))
})

test_that("birch_dw.df test size and names", {
  expect_identical(nrow(birch_dw.df), 700L)
  expect_identical(ncol(birch_dw.df), 5L)
  expect_named(birch_dw.df,
               c("Container", "Density", "block", "dry.weight", "Part"))
})
