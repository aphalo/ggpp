context("stat_fit_glance")

library(tibble)
library(gginnards)
library(nlme)
library(quantreg)
library(broom)
library(broom.mixed)

set.seed(4321)
# generate artificial data
x <- 1:100
y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
my.data <- data.frame(x,
                      y,
                      group = c("A", "B"),
                      y2 = y * c(0.5,2),
                      block = c("a", "a", "b", "b"),
                      wt = sqrt(x))
                      formula <- y ~ poly(x, 3, raw = TRUE)


test_that("glance_methods", {
  # ggplot(my.data, aes(x, y)) +
  #   geom_point() +
  #   stat_fit_glance(geom = "debug")
  #
  # ggplot(my.data, aes(x, y)) +
  #   geom_point() +
  #   stat_fit_glance(method = "rq", geom = "debug")
  #
  # ggplot(my.data, aes(x, y)) +
  #   geom_point() +
  #   stat_fit_glance(method = "cor.test", method.args = list(formula = ~ x + y), geom = "debug")
  #
  # ggplot(my.data, aes(x, y)) +
  #   geom_point() +
  #   stat_fit_glance(method = "cor.test", method.arg = list(x = "x", y = "y"), geom = "debug")

  vdiffr::expect_doppelganger("glance_method_default",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_glance(mapping =
                                  aes(label = sprintf("%.3g, %.3f, %.3f, %.3g, %.3g, %.3g",
                                                    stat(p.value),
                                                    stat(r.squared),
                                                    stat(adj.r.squared),
                                                    stat(AIC),
                                                    stat(BIC),
                                                    stat(df.residual))))
  )

  vdiffr::expect_doppelganger("glance_method_lm_fun",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_glance(method = "lm",
                                                mapping =
                                                  aes(label = sprintf("%.3g, %.3f, %.3f, %.3g, %.3g, %.3g",
                                                                      stat(p.value),
                                                                      stat(r.squared),
                                                                      stat(adj.r.squared),
                                                                      stat(AIC),
                                                                      stat(BIC),
                                                                      stat(df.residual))))
  )

  vdiffr::expect_doppelganger("glance_method_lm_char",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_glance(method = "lm",
                                                mapping =
                                                  aes(label = sprintf("%.3g, %.3f, %.3f, %.3g, %.3g, %.3g",
                                                                      stat(p.value),
                                                                      stat(r.squared),
                                                                      stat(adj.r.squared),
                                                                      stat(AIC),
                                                                      stat(BIC),
                                                                      stat(df.residual))))
  )

  vdiffr::expect_doppelganger("glance_method_args",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_glance(method = "lm",
                                                method.args = list(formula = y ~ x + I(x^2)),
                                                mapping =
                                                  aes(label = sprintf("%.3g, %.3f, %.3f, %.3g, %.3g, %.3g",
                                                                      stat(p.value),
                                                                      stat(r.squared),
                                                                      stat(adj.r.squared),
                                                                      stat(AIC),
                                                                      stat(BIC),
                                                                      stat(df.residual))))
  )

  old.options <- options(warn = -1)
  vdiffr::expect_doppelganger("glance_method_rq",
                               ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_glance(method = "rq",
                                                method.args = list(formula = y ~ x + I(x^2)),
                                                mapping =
                                                  aes(label = sprintf("%.3g, %.3g, %.3g, %.3g, %.3g",
                                                                      stat(tau),
                                                                      stat(logLik),
                                                                      stat(AIC),
                                                                      stat(BIC),
                                                                      stat(df.residual))))
   )
  options(old.options)

  vdiffr::expect_doppelganger("glance_method_cortest_xy",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_glance(method = "cor.test",
                                                method.args = list(x = "x", y = "y"),
                                                mapping =
                                                  aes(label = sprintf("%.3g, %.3g",
                                                                      stat(estimate),
                                                                      stat(p.value))))
  )

  vdiffr::expect_doppelganger("glance_method_cortest_formula",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_glance(method = "cor.test",
                                                method.args = list(formula = ~ x + y),
                                                mapping =
                                                  aes(label = sprintf("%.3g, %.3g",
                                                                      stat(estimate),
                                                                      stat(p.value))))
  )

})

test_that("tidy_methods", {
  # ggplot(my.data, aes(x, y)) +
  #   geom_point() +
  #   stat_fit_tidy(geom = "debug")
  #
  # ggplot(my.data, aes(x, y)) +
  #   geom_point() +
  #   stat_fit_tidy(method = "rq", geom = "debug")
  #
  # ggplot(my.data, aes(x, y)) +
  #   geom_point() +
  #   stat_fit_tidy(method = "rq", tidy.args = list(se = "nid"), geom = "debug")

  vdiffr::expect_doppelganger("tidy_method_default",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_tidy(mapping =
                                                  aes(label = sprintf("%.3g, %.3g, %.3g, %.3g\n%.3g, %.3g, %.3g, %.3g",
                                                                      stat(Intercept_estimate),
                                                                      stat(Intercept_p.value),
                                                                      stat(Intercept_stat),
                                                                      stat(Intercept_se),
                                                                      stat(x_estimate),
                                                                      stat(x_p.value),
                                                                      stat(x_stat),
                                                                      stat(x_se))))
  )

  vdiffr::expect_doppelganger("tidy_method_lm_fun",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_tidy(method = lm,
                                              mapping =
                                                aes(label = sprintf("%.3g, %.3g, %.3g, %.3g\n%.3g, %.3g, %.3g, %.3g",
                                                                    stat(Intercept_estimate),
                                                                    stat(Intercept_p.value),
                                                                    stat(Intercept_stat),
                                                                    stat(Intercept_se),
                                                                    stat(x_estimate),
                                                                    stat(x_p.value),
                                                                    stat(x_stat),
                                                                    stat(x_se))))
  )

  vdiffr::expect_doppelganger("tidy_method_lm_char",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_tidy(method = "lm",
                                              mapping =
                                                aes(label = sprintf("%.3g, %.3g, %.3g, %.3g\n%.3g, %.3g, %.3g, %.3g",
                                                                    stat(Intercept_estimate),
                                                                    stat(Intercept_p.value),
                                                                    stat(Intercept_stat),
                                                                    stat(Intercept_se),
                                                                    stat(x_estimate),
                                                                    stat(x_p.value),
                                                                    stat(x_stat),
                                                                    stat(x_se))))
  )

  vdiffr::expect_doppelganger("tidy_method_args",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_tidy(method = "lm",
                                              method.args = list(formula = y ~ x + I(x^2)),
                                              mapping =
                                                aes(label = sprintf("%.3g, %.3g, %.3g, %.3g\n%.3g, %.3g, %.3g, %.3g",
                                                                    stat(Intercept_estimate),
                                                                    stat(Intercept_p.value),
                                                                    stat(Intercept_stat),
                                                                    stat(Intercept_se),
                                                                    stat(x_estimate),
                                                                    stat(x_p.value),
                                                                    stat(x_stat),
                                                                    stat(x_se))))
  )

  old.options <- options(warn = -1)
  vdiffr::expect_doppelganger("tidy_tidy_args",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_tidy(method = "rq",
                                              tidy.args = list(se.type = "nid"),
                                              mapping =
                                                aes(label = sprintf("%.3g, %.3g, %.3g, %.3g\n%.3g, %.3g, %.3g, %.3g",
                                                                    stat(Intercept_estimate),
                                                                    stat(Intercept_p.value),
                                                                    stat(Intercept_stat),
                                                                    stat(Intercept_se),
                                                                    stat(x_estimate),
                                                                    stat(x_p.value),
                                                                    stat(x_stat),
                                                                    stat(x_se))))
  )

  vdiffr::expect_doppelganger("tidy_method_rq",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_tidy(method = "rq",
                                              mapping =
                                                aes(label = sprintf("%.3g, %.3g, %.3g, %.3g\n%.3g, %.3g, %.3g, %.3g",
                                                                    stat(Intercept_estimate),
                                                                    stat(Intercept_conf.low),
                                                                    stat(Intercept_conf.high),
                                                                    stat(Intercept_tau),
                                                                    stat(x_estimate),
                                                                    stat(x_conf.low),
                                                                    stat(x_conf.high),
                                                                    stat(x_tau))))
  )
  options(old.options)

})

test_that("augment_methods", {
  # ggplot(my.data, aes(x, y)) +
  #   geom_point() +
  #   stat_fit_augment(geom = "debug")

  vdiffr::expect_doppelganger("augment_method_default",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_augment()
  )

  vdiffr::expect_doppelganger("augment_method_lm_fun",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_augment(method = lm)
  )

  vdiffr::expect_doppelganger("augment_method_lm_char",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_augment(method = "lm")
  )

  vdiffr::expect_doppelganger("augment_method_args",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_augment(method = "lm",
                                                 method.args = list(formula = y ~ x + I(x^2)))
  )

  old.options <- options(warn = -1)
  vdiffr::expect_doppelganger("augment_method_rq",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_augment(method = "rq")
  )

  vdiffr::expect_doppelganger("augment_rqmethod__args",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_augment(method = "rq",
                                                 method.args = list(formula = y ~ x + I(x^2)))
  )
  options(old.options)

})
