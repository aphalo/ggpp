context("geom_label_npc")

df <- data.frame(
x = c(0, 0, 1, 1, 0.5),
x.chr = c("left", "left", "right", "right", "center"),
y = c(0, 1, 0, 1, 0.5),
y.chr = c("bottom", "top", "bottom", "top", "middle"),
text = c("bottom-left", "top-left", "bottom-right", "top-right", "center-middle")
)

test_that("geom_label_npc, runs successfully", {
  expect_no_error(
    ggplot(data = mtcars) +
    geom_point(mapping = aes(wt, mpg)) +
    geom_label_npc(data = df, aes(npcx = x, npcy = y, label = text))
  )
})

test_that("geom_label_npc pos_or_nudge", {
  expect_error(
    geom_label_npc(position = "", nudge_x = 0.5),
    "You must specify either `position` or `nudge_x`/`nudge_y`."
  )
})
