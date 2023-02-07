context("geom_label_s")

my.cars <- mtcars[c(TRUE, FALSE, FALSE, FALSE), ]
my.cars$name <- rownames(my.cars)

p <- ggplot(my.cars, aes(wt, mpg, label = name)) +
geom_point(color = "red")

test_that("geom_label_s runs successfully", {
  expect_no_error(
    p +
      geom_label_s(nudge_x = 0.12)
  )
})

test_that("geom_label_s pos_or_nudge", {
  expect_error(
    p +
      geom_label_s(nudge_x = 0.12, position = ""),
    "You must specify either `position` or `nudge_x`/`nudge_y`."
  )
})
