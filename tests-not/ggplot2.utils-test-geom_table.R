library(dplyr)
library(tibble)

get_tb <- function() {
  mtcars %>%
    group_by(cyl) %>%
    summarize(wt = mean(wt), mpg = mean(mpg)) %>%
    ungroup() %>%
    mutate(
      wt = sprintf("%.2f", wt),
      mpg = sprintf("%.1f", mpg)
    )
}

test_that("geom_table works as expected", {
  p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
    geom_point()
  tb <- get_tb()
  df <- tibble(x = 5.45, y = 34, tb = list(tb))
  result <- expect_silent(
    p + geom_table(data = df, aes(x = x, y = y, label = tb))
  )
  expect_s3_class(result, "ggplot")
})

test_that("geom_table_npc works as expected", {
  p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
    geom_point()
  tb <- get_tb()
  dfnpc <- tibble(x = 0.95, y = 0.95, tb = list(tb))
  result <- expect_silent(
    p + geom_table_npc(data = dfnpc, aes(npcx = x, npcy = y, label = tb))
  )
  expect_s3_class(result, "ggplot")
})
