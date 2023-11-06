context("RNAseq_data")

test_that("volcano_example.df test size and names", {
expect_identical(nrow(volcano_example.df), 1218L)
expect_identical(ncol(volcano_example.df), 6L)
expect_named(volcano_example.df,
             c("tag", "gene", "outcome", "logFC", "PValue", "genotype"))
})

test_that("quadrant_example.df test size and names", {
  expect_identical(nrow(quadrant_example.df), 1015L)
  expect_identical(ncol(quadrant_example.df), 7L)
  expect_named(quadrant_example.df,
               c("tag", "gene", "outcome.x", "outcome.y",
                 "logFC.x", "logFC.y", "genotype"))
})
