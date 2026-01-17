context("positions")

test_that("stacknudge", {
  df <- data.frame(x1 = c(1, 2, 1, 3, -1),
                   x2 = c("a", "a", "b", "b", "b"),
                   grp = c("some long name", "other name", "some name",
                           "another name", "some long name"))

  p <-
    ggplot(data = df, aes(x1, x2, group = grp)) +
    geom_col(aes(fill = grp), width = 0.5) +
    theme(legend.position = "none")

  vdiffr::expect_doppelganger("stack_nudge1",
                              p + geom_text(
                                aes(label = grp),
                                position = position_stacknudge(vjust = 0.5, y = 0.3))
  )
  vdiffr::expect_doppelganger("stack_nudge2",
                              p + geom_text_s(
                                aes(label = grp),
                                vjust = 0,
                                position = position_stacknudge(vjust = 0.5, y = 0.3))
  )
  vdiffr::expect_doppelganger("stack_nudge3",
                              p + geom_text(
                                aes(label = grp),
                                hjust = 0,
                                position = position_stacknudge(vjust = 0, x = 0.1))
  )
  vdiffr::expect_doppelganger("stack_nudge4",
                              p + geom_text(
                                aes(label = grp),
                                hjust = 0, angle = 90,
                                position = position_stacknudge(vjust = 0, x = 0.1, y = -0.21))
  )
  vdiffr::expect_doppelganger("stack_nudge5",
                              p + geom_text_s(
                                aes(label = grp),
                                vjust = 0.5,
                                box.padding = 0.99,
                                position = position_stacknudge(vjust = 0.5, y = c(0.3, -0.3)))
  )
  vdiffr::expect_doppelganger("stack_nudge6",
                              p + geom_text_s(
                                aes(label = grp),
                                vjust = 0.5,
                                box.padding = 0.99,
                                position = position_stacknudge(vjust = 0.5,
                                                               y = c(rep(0.3, 2), rep(-0.3, 3))))
  )
  vdiffr::expect_doppelganger("stack_nudge7",
                              ggplot(data = df, aes(x1, x2, group = grp)) +
                                geom_col(aes(fill = grp),
                                         width = 0.5,
                                         position = position_stack(reverse = TRUE)) +
                                theme(legend.position = "none") +
                                geom_text(
                                  aes(label = grp),
                                  position = position_stacknudge(vjust = 0.5,
                                                                 y = 0.3,
                                                                 reverse = TRUE))
  )
})

test_that("fillnudge", {
  df <- data.frame(x1 = c(1, 2, 1, 3, -1),
                   x2 = c("a", "a", "b", "b", "b"),
                   grp = c("some long name", "other name", "some name",
                           "another name", "some long name"))

  p <-
    ggplot(data = df, aes(x1, x2, group = grp)) +
    geom_col(aes(fill = grp), width = 0.5, position = "fill") +
    theme(legend.position = "none")

  vdiffr::expect_doppelganger("fill_nudge1",
                              p + geom_text(aes(label = grp),
                                            position = position_fillnudge(vjust = 0.5, y = 0.3))
  )
  vdiffr::expect_doppelganger("fill_nudge2",
                              p + geom_text_s(aes(label = grp),
                                              vjust = 0,
                                              position = position_fillnudge(vjust = 0.5, y = 0.3))
  )
  vdiffr::expect_doppelganger("fill_nudge3",
                              p + geom_text(aes(label = grp),
                                            angle = 90,
                                            position = position_fillnudge(vjust = 0, x = 0.1))
  )
  vdiffr::expect_doppelganger("fill_nudge4",
                              p + geom_text(aes(label = grp),
                                            hjust = 0, angle = 90,
                                            position = position_fillnudge(vjust = 0, x = 0.1, y = -0.21))
  )
  vdiffr::expect_doppelganger("fill_nudge5",
                              p + geom_text_s(aes(label = grp),
                                              vjust = 0.5,
                                              box.padding = 0.99,
                                              position = position_fillnudge(vjust = 0.5, y = c(0.3, -0.3)))
  )
  vdiffr::expect_doppelganger("fill_nudge6",
                              p + geom_text_s(aes(label = grp),
                                              vjust = 0.5,
                                              box.padding = 0.99,
                                              position = position_fillnudge(vjust = 0.5,
                                                                             y = c(rep(0.3, 2), rep(-0.3, 3))))
  )
  vdiffr::expect_doppelganger("fill_nudge7",
                              ggplot(data = df, aes(x1, x2, group = grp)) +
                                geom_col(aes(fill = grp),
                                         width = 0.5,
                                         position = position_fill(reverse = TRUE)) +
                                theme(legend.position = "none") +
                                geom_text(aes(label = grp),
                                          position = position_fillnudge(vjust = 0.5,
                                                                        y = 0.3,
                                                                        reverse = TRUE))
  )
})

test_that("dodgenudge", {
  df <- data.frame(x1 = c(1, 2, 1, 3, -1),
                   x2 = c("a", "a", "b", "b", "b"),
                   grp = c("some long name", "other name", "some name",
                           "another name", "some long name"))

  p <-
    ggplot(data = df, aes(x2, x1, group = grp)) +
    geom_col(aes(fill = grp),
             width=0.5,
             position = position_dodge(width = 0.5)) +
    theme(legend.position = "none")

  vdiffr::expect_doppelganger("dodge_nudge1",
                              p + geom_text(
                                aes(label = grp),
                                position = position_dodgenudge(width = 0.5, y = 0.1))
  )
  vdiffr::expect_doppelganger("dodge_nudge2",
                              p + geom_text(aes(label = grp),
                                            position = position_dodgenudge(width = 0.5, y = 0.1,
                                                                           direction = "split"))
  )
  vdiffr::expect_doppelganger("dodge_nudge3",
                              p + geom_text_s(aes(label = grp),
                                              hjust = 0,
                                              position = position_dodgenudge(width = 0.5, y = 0.2,
                                                                             direction = "split"))
  )
  vdiffr::expect_doppelganger("dodge_nudge4",
                              p + geom_text(
                                aes(label = grp),
                                hjust = "center",
                                position = position_dodgenudge(width = 0.5, y = -0.075,
                                                               direction = "split"))
  )
  # assumes the default order of factor levels
  vdiffr::expect_doppelganger("dodge_nudge5",
                              p + geom_text(
                                aes(label = grp),
                                position = position_dodgenudge(width = 0.5,
                                                               y = c(rep(0.1, 4), -0.1)))
  )
  vdiffr::expect_doppelganger("dodge_nudge6",
                              p + geom_text(aes(label = grp),
                                            position = position_dodgenudge(width = 0.5,
                                                                           y = 0.1))
  )
  skip_if(utils::packageVersion("ggplot2") < "4.0.0", "'ggplot2' < 4.0.0")
  vdiffr::expect_doppelganger("dodge_nudge7",
                              ggplot(data = df, aes(x2, x1, group = grp)) +
                                geom_col(
                                  aes(fill = grp),
                                  width=0.5,
                                  position = position_dodge(width = 0.5,
                                                            reverse = TRUE)) +
                                theme(legend.position = "none") +
                                geom_text(
                                  aes(label = grp),
                                  position = position_dodgenudge(width = 0.5,
                                                                 y = 0.1,
                                                                 reverse = TRUE))
  )
})

test_that("nudge_keep", {
  df <- data.frame(x1 = c(1, 2, 1, 3, -1),
                   x2 = c("a", "b", "c", "d", "e"),
                   grp = c("some long name", "other name", "some name",
                           "another name", "some long name"))

  p <-
    ggplot(data = df, aes(x2, x1, group = grp)) +
    geom_col(aes(fill = grp)) +
    theme(legend.position = "none")

  vdiffr::expect_doppelganger("nudge_keep1",
                              p + geom_text_s(
                                aes(label = grp),
                                vjust = 0, hjust = 0.5,
                                position = position_nudge_keep(y = 0.12))
  )
  vdiffr::expect_doppelganger("nudge_keep2",
                              p + geom_text_s(
                                aes(label = grp),
                                vjust = 0, hjust = "left",
                                position = position_nudge_keep(y = 0.12))
  )
  vdiffr::expect_doppelganger("nudge_keep3",
                              p + geom_text_s(
                                aes(label = grp),
                                vjust = 0, hjust = "inward",
                                position = position_nudge_keep(y = 0.12))
  )
  vdiffr::expect_doppelganger("nudge_keep4",
                              p + geom_text_s(
                                aes(label = grp),
                                position = position_nudge_keep(y = 0.12))
  )
  vdiffr::expect_doppelganger("nudge_keep5",
                              p + geom_text_s(
                                aes(label = grp),
                                position = position_nudge_keep(y = c(rep(0.12, 4), -0.12)))
  )
  vdiffr::expect_doppelganger("nudge_keep6",
                              p + geom_text_s(
                                aes(label = grp),
                                position = position_nudge_keep(y = c(rep(0.12, 4), -0.12),
                                                               obey_grouping = TRUE))
  )
  vdiffr::expect_doppelganger("nudge_keep7",
                              p + geom_text_s(
                                aes(label = grp),
                                position = position_nudge_keep(y = c(0.12, -0.12)))
  )
})

test_that("nudge_keep works with duration", {
  df <- data.frame(x1 = lubridate::as.duration(c(1, 2, 1, 3, -1)),
                   x2 = c("a", "b", "c", "d", "e"),
                   grp = c("some long name", "other name", "some name",
                           "another name", "some long name"))

  p <-
    ggplot(data = df, aes(x2, x1, group = grp)) +
    geom_col(aes(fill = grp)) +
    theme(legend.position = "none")

  vdiffr::expect_doppelganger("nudge_keep_duration1",
                              p + geom_text_s(
                                aes(label = grp),
                                vjust = 0, hjust = 0.5,
                                position = position_nudge_keep(y = 0.12))
  )
  vdiffr::expect_doppelganger("nudge_keep_duration2",
                              p + geom_text_s(
                                aes(label = grp),
                                vjust = 0, hjust = "left",
                                position = position_nudge_keep(y = 0.12))
  )
})

test_that("nudge_center", {
  df <- data.frame(
    x = c(1,3,2,5,4,2.5),
    y = c("abc","cd","d","c","bcd","a")
  )

  vdiffr::expect_doppelganger("nudge_center0",
                              ggplot(df, aes(x, y, label = y)) +
                                geom_point() +
                                geom_text(
                                  position = position_nudge_center(center_x = 3, center_y = 4,
                                                                   x = 0.1, y = 0.2)
                                )
  )
  vdiffr::expect_doppelganger("nudge_center1",
                              ggplot(df, aes(x, y, label = y)) +
                                geom_point() +
                                geom_text(hjust = 0, vjust = 0,
                                          position = position_nudge_center(x = 0.05,
                                                                           y = 0.07)
                                )
  )
  vdiffr::expect_doppelganger("nudge_center1a",
                              ggplot(df, aes(x, y, label = y)) +
                                geom_point() +
                                geom_text(hjust = 0, vjust = 0,
                                          position = position_nudge_center(x = 0.05,
                                                                           y = 0.07,
                                                                           direction = "none")
                                )
  )
  vdiffr::expect_doppelganger("nudge_center1b",
                              ggplot(df, aes(x, y, label = y)) +
                                geom_point() +
                                geom_text(hjust = 0, vjust = 0,
                                          position = position_nudge_center(x = c(0.05, -0.05),
                                                                           y = 0.07)
                                )
  )
  vdiffr::expect_doppelganger("nudge_center1c",
                              ggplot(df, aes(x, y, label = y)) +
                                geom_point() +
                                geom_text(hjust = 0, vjust = 0,
                                          position = position_nudge_center(x = 0.05,
                                                                           y = c(0.1, -0.1))
                                )
  )
  vdiffr::expect_doppelganger("nudge_center2",
                              ggplot(df, aes(x, y)) +
                                geom_point() +
                                geom_text(aes(label = y),
                                          hjust = "outward", vjust = "outward",
                                          position = position_nudge_center(x = 0.05,
                                                                           y = 0.07,
                                                                           direction = "split"))
  )
  vdiffr::expect_doppelganger("nudge_center3",
                              ggplot(df, aes(x, y)) +
                                geom_point() +
                                geom_text(aes(label = y),
                                          hjust = "outward", vjust = "outward",
                                          position = position_nudge_center(x = 0.05,
                                                                           y = 0.07,
                                                                           direction = "radial"))
  )
  vdiffr::expect_doppelganger("nudge_center4",
                              ggplot(df, aes(x, y)) +
                                geom_point() +
                                geom_text(aes(label = y),
                                          vjust = "outward", hjust = "outward",
                                          position = position_nudge_center(x = 0.06,
                                                                           y = 0.08,
                                                                           center_y = 2,
                                                                           center_x = 1.5,
                                                                           direction = "split"))
  )
  vdiffr::expect_doppelganger("nudge_center5",
                              ggplot(df, aes(x, y)) +
                                geom_point() +
                                geom_text(aes(label = y),
                                          vjust = "outward", hjust = "outward",
                                          position = position_nudge_center(x = 0.06,
                                                                           y = 0.08,
                                                                           center_y = 2,
                                                                           center_x = 1.5,
                                                                           direction = "radial"))
  )
  vdiffr::expect_doppelganger("nudge_center6",
                              ggplot(df, aes(x, y)) +
                                geom_point() +
                                geom_text(aes(label = y),
                                          vjust = "outward", hjust = "outward",
                                          position = position_nudge_center(x = 0.06,
                                                                           y = 0.08,
                                                                           center_y = 2))
  )
  vdiffr::expect_doppelganger("nudge_center7",
                              ggplot(df, aes(x, y)) +
                                geom_point() +
                                geom_text(aes(label = y),
                                          vjust = "outward", hjust = "outward",
                                          position = position_nudge_center(x = 0.06,
                                                                           y = 0.08,
                                                                           center_x = 1.5))
                                )
})

test_that("nudge_center works with time", {
  df <- data.frame(
    x = lubridate::dmy("01-01-1070") + lubridate::hours(c(1,3,2,5,4,6)),
    y = c("abc","cd","d","c","bcd","a")
  )

  vdiffr::expect_doppelganger("nudge_center_time0",
                              ggplot(df, aes(x, y, label = y)) +
                                geom_point() +
                                geom_text(
                                  position = position_nudge_center(center_y = 4,
                                                                   x = 600, # numeric = seconds
                                                                   y = 0.2)
                                )
  )
  vdiffr::expect_doppelganger("nudge_center_time1",
                              ggplot(df, aes(x, y, label = y)) +
                                geom_point() +
                                geom_text(hjust = 0, vjust = 0,
                                          position = position_nudge_center(x = 300, # numeric = seconds
                                                                           y = 0.07)
                                )
  )
})

test_that("nudge_center works with duration", {
  df <- data.frame(
    x = lubridate::as.duration(lubridate::hours(c(1,3,2,5,4,6))),
    y = c("abc","cd","d","c","bcd","a")
  )

  vdiffr::expect_doppelganger("nudge_center_duration0",
                              ggplot(df, aes(x, y, label = y)) +
                                geom_point() +
                                geom_text(
                                  position = position_nudge_center(x = 300, # numeric = seconds
                                                                   y = 0.2)
                                ) # +
 #                               scale_x_time() # depends on package 'hms'
  )
  vdiffr::expect_doppelganger("nudge_center_duration1",
                              ggplot(df, aes(x, y, label = y)) +
                                geom_point() +
                                geom_text(hjust = 0, vjust = 0,
                                          position = position_nudge_center(x = 300, # numeric = seconds
                                                                           y = 0.07)
                                ) # +
#                                scale_x_time() # depends on package 'hms'
  )
})

test_that("nudge_line", {
  set.seed(16532)
  df <- data.frame(
    x = -10:10,
    y = (-10:10)^2,
    yy = (-10:10)^2 + rnorm(21, 0, 4),
    yyy = (-10:10) + rnorm(21, 0, 4),
    l = letters[1:21]
  )

  expect_error(ggplot(df, aes(x, y, label = l)) +
                  geom_text(position = position_nudge_line(kept.origin = "")))
  expect_error(ggplot(df, aes(x, y, label = l)) +
                 geom_text(position = position_nudge_line(kept.origin = "zz")))

  { # output under linux does not match snapshots created under Windows
    skip_on_os(c("mac", "linux", "solaris"))
    vdiffr::expect_doppelganger("nudge_line1",
                                ggplot(df, aes(x, y, label = l)) +
                                  geom_line() +
                                  geom_point() +
                                  geom_text(position = position_nudge_line())
    )
    vdiffr::expect_doppelganger("nudge_line2",
                                ggplot(df, aes(x, y, label = l)) +
                                  geom_line() +
                                  geom_point() +
                                  geom_text(position = position_nudge_line(xy_relative = -0.03))
    )
  }

  vdiffr::expect_doppelganger("nudge_line3",
                              ggplot(df, aes(x, x * 2 + 5, label = l)) +
                                geom_abline(intercept = 5, slope = 2) +
                                geom_point() +
                                geom_text(position = position_nudge_line(abline = c(5, 2)))
  )
  vdiffr::expect_doppelganger("nudge_line3a",
                              ggplot(df, aes(x, x * 2 + 5, label = l)) +
                                geom_abline(intercept = 5, slope = 2) +
                                geom_point() +
                                geom_text(position = position_nudge_line(abline = c(5, 2),
                                                                         x = c(-0.5, 0.5),
                                                                         y = c(-0.5, 0.5)))
  )
  vdiffr::expect_doppelganger("nudge_line4",
                              ggplot(subset(df, x >= 0), aes(x, yyy)) +
                                geom_point() +
                                geom_text(aes(label = l),
                                          position = position_nudge_line(direction = "split"))
  )
  vdiffr::expect_doppelganger("nudge_line5",
                              ggplot(subset(df, x >= 0), aes(y, yy)) +
                                stat_smooth(method = "lm", formula = y ~ x) +
                                geom_point() +
                                geom_text(aes(label = l),
                                          position = position_nudge_line(method = "lm",
                                                                         direction = "split"))
  )
  vdiffr::expect_doppelganger("nudge_line6",
                              ggplot(subset(df, x >= 0), aes(y, yy)) +
                                stat_smooth(method = "lm", formula = y ~ x) +
                                geom_point() +
                                geom_text(aes(label = l),
                                          position = position_nudge_line(method = "linear",
                                                                         direction = "split"))
  )
  vdiffr::expect_doppelganger("nudge_line7",
                              ggplot(subset(df, x >= 0), aes(y, yy)) +
                                stat_smooth(method = "lm", formula = y ~ x) +
                                geom_point() +
                                geom_text(aes(label = l),
                                          position = position_nudge_line(line_nudge = 1.5))
  )
})

test_that("nudge_to", {
  df <- data.frame(
    x = c(1,3,2,5,4,2.5),
    y = c(2, 1, 2.5, 1.8, 2.8, 1.5),
    label = c("abc","cd","d","c","bcd","a")
  )

  # errors ------------------------------------------------------------------

  expect_error(
    ggplot(df, aes(x, y, label = label)) +
      geom_text_s(position =
                    position_nudge_to(y = 3,
                                      kept.origin = "wrong value"))
  )
  expect_error(
    ggplot(df, aes(x, y, label = label)) +
      geom_text_s(position =
                    position_nudge_to(y = 3,
                                      x.action = "wrong value"))
  )
  expect_error(
    ggplot(df, aes(x, y, label = label)) +
      geom_text_s(position =
                    position_nudge_to(y = 3,
                                      y.action = "wrong value"))
  )
  expect_error(
    ggplot(df, aes(x, y, label = label)) +
      geom_point() +
      geom_text_s(position =
                    position_nudge_to(x = NA_real_,
                                      y.action = "spread"))
  )
  expect_error(
    ggplot(df, aes(x, y, label = label)) +
      geom_point() +
      geom_text_s(position =
                    position_nudge_to(x = NA,
                                      y.action = "spread"))
  )

  expect_no_error(
    ggplot(df, aes(x, y, label = label)) +
      geom_text_s(position =
                    position_nudge_to(y = 3,
                                      kept.origin = "none"))
  )
  expect_no_error(
    ggplot(df, aes(x, y, label = label)) +
      geom_text_s(position =
                    position_nudge_to(y = 3,
                                      x.action = "none"))
  )
  expect_no_error(
    ggplot(df, aes(x, y, label = label)) +
      geom_text_s(position =
                    position_nudge_to(y = 3,
                                      y.action = "none"))
  )


  expect_no_error(
    ggplot(df, aes(x, y, label = label)) +
                    geom_text_s(position =
                                  position_nudge_to(y = 3,
                                                    kept.origin = "original"))
    )
  expect_no_error(
    ggplot(df, aes(x, y, label = label)) +
                    geom_point() +
                    geom_text_s(position =
                                  position_nudge_to(y = 3,
                                                    x.action = "spread"))
    )
  expect_no_error(
    ggplot(df, aes(x, y, label = label)) +
      geom_point() +
      geom_text(position =
                    position_nudge_to(x = 4,
                                      y.action = "spread"))
  )
  expect_no_error(
    ggplot(df, aes(x, y, label = label)) +
      geom_point() +
      geom_text_s(position =
                  position_nudge_to(x = 0,
                                    y.action = "spread"))
  )


# warnings ----------------------------------------------------------------

  ## works outside testthat, but warning seems to be muted by testthat
  # expect_warning(
  #   ggplot(df, aes(x, y, label = label)) +
  #     geom_point() +
  #     geom_text_s(position =
  #                   position_nudge_to(x = rep(0, 7),
  #                                     y.action = "spread"))
  # )

  expect_no_warning(
    ggplot(df, aes(x, y, label = label)) +
      geom_point() +
      geom_text_s(position =
                    position_nudge_to(x = rep(0, 6),
                                      y.action = "spread"))
  )

  expect_no_warning(
    ggplot(df, aes(x, y, label = label)) +
      geom_point() +
      geom_text_s(position =
                    position_nudge_to(x = NULL,
                                      y.action = "spread"))
  )

  expect_no_warning(
    ggplot(df, aes(x, y, label = label)) +
      geom_point() +
      geom_text_s(position =
                    position_nudge_to(x = numeric(0),
                                      y.action = "spread"))
  )

  # x.action = none, y.action = none -----------------------------------------

  vdiffr::expect_doppelganger("nudge_to1",
                              ggplot(df, aes(x, y, label = label)) +
                                geom_point() +
                                geom_text_s(position = position_nudge_to(y = 3),
                                            vjust = 0)
  )
  vdiffr::expect_doppelganger("nudge_to1a",
                              ggplot(df, aes(x, y, label = label)) +
                                geom_point() +
                                geom_text_s(position = position_nudge_to(y = 3 + (1:6) / 6),
                                            vjust = 0)
  )
  vdiffr::expect_doppelganger("nudge_to2",
                              ggplot(df, aes(x, y, label = label)) +
                                geom_point() +
                                geom_text_s(position = position_nudge_to(x = 6),
                                            hjust = 0)
  )

  # x.action = spread, y.action = none --------------------------------------

  vdiffr::expect_doppelganger("nudge_to_spread_x1",
                              ggplot(df, aes(x, y, label = label)) +
                                geom_point() +
                                geom_text_s(position = position_nudge_to(x.action = "spread", y = 3),
                                            hjust = 0)
  )
  vdiffr::expect_doppelganger("nudge_to_expand_x1",
                              ggplot(df, aes(x, y, label = label)) +
                                geom_point() +
                                geom_text_s(position = position_nudge_to(x.action = "spread",
                                                                         y = 3,
                                                                         x.expansion = 0.1),
                                            hjust = 0)
  )
  vdiffr::expect_doppelganger("nudge_to_contract_x1",
                              ggplot(df, aes(x, y, label = label)) +
                                geom_point() +
                                geom_text_s(position = position_nudge_to(x.action = "spread",
                                                                         y = 3,
                                                                         x.expansion = -0.1),
                                            hjust = 0)
  )
  vdiffr::expect_doppelganger("nudge_to_contract_expand_x1",
                              ggplot(df, aes(x, y, label = label)) +
                                geom_point() +
                                geom_text_s(position = position_nudge_to(x.action = "spread",
                                                                         y = 3,
                                                                         x.expansion = c(-0.1, 0.1)),
                                            hjust = 0)
  )
  vdiffr::expect_doppelganger("nudge_to_spread_x2",
                              ggplot(df, aes(x, y, label = label)) +
                                geom_point() +
                                geom_text_s(position = position_nudge_to(x.action = "spread",
                                                                         x = c(2, 4), y = 3),
                                            hjust = 0)
  )
  vdiffr::expect_doppelganger("nudge_to_expand_x2",
                              ggplot(df, aes(x, y, label = label)) +
                                geom_point() +
                                geom_text_s(position = position_nudge_to(x.action = "spread",
                                                                         x = c(2, 4), y = 3,
                                                                         x.expansion = 0.1),
                                            hjust = 0)
  )

  # x.action = none, y.action = spread --------------------------------------

  vdiffr::expect_doppelganger("nudge_to_spread_y1",
                              ggplot(df, aes(x, y, label = label)) +
                                geom_point() +
                                geom_text_s(position = position_nudge_to(x = 6, y.action = "spread"),
                                            hjust = 0)
  )
  vdiffr::expect_doppelganger("nudge_to_expand_y1",
                              ggplot(df, aes(x, y, label = label)) +
                                geom_point() +
                                geom_text_s(position = position_nudge_to(x = 6,
                                                                         y.action = "spread",
                                                                         y.expansion = 0.1),
                                            hjust = 0)
  )
  vdiffr::expect_doppelganger("nudge_to_contract_y1",
                              ggplot(df, aes(x, y, label = label)) +
                                geom_point() +
                                geom_text_s(position = position_nudge_to(x = 6,
                                                                         y.action = "spread",
                                                                         y.expansion = -0.1),
                                            hjust = 0)
  )
  vdiffr::expect_doppelganger("nudge_to_contract_expand_y1",
                              ggplot(df, aes(x, y, label = label)) +
                                geom_point() +
                                geom_text_s(position = position_nudge_to(x = 6,
                                                                         y.action = "spread",
                                                                         y.expansion = c(-0.1, 0.1)),
                                            hjust = 0)
  )
  vdiffr::expect_doppelganger("nudge_to_spread_y2",
                              ggplot(df, aes(x, y, label = label)) +
                                geom_point() +
                                geom_text_s(position = position_nudge_to(x = 6, y = c(1, 3),
                                                                         y.action = "spread"),
                                            hjust = 0)
  )
  vdiffr::expect_doppelganger("nudge_to_expand_y2",
                              ggplot(df, aes(x, y, label = label)) +
                                geom_point() +
                                geom_text_s(position = position_nudge_to(x = 6, y = c(1, 3),
                                                                         y.action = "spread",
                                                                         y.expansion = 0.1),
                                            hjust = 0)
  )

  # x.action = spread, y.action = spread ------------------------------------

  vdiffr::expect_doppelganger("nudge_to_spread_x1y1",
                              ggplot(df, aes(x, y, label = label)) +
                                geom_point() +
                                geom_text_s(position = position_nudge_to(x.action = "spread",
                                                                         y.action = "spread"))
  )
  vdiffr::expect_doppelganger("nudge_to_expand_x1y1",
                              ggplot(df, aes(x, y, label = label)) +
                                geom_point() +
                                geom_text_s(position = position_nudge_to(x.action = "spread",
                                                                         y.action = "spread",
                                                                         x.expansion = 0.05,
                                                                         y.expansion = 0.05))
  )
})
