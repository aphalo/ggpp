#' # Linear regression using a table theme
#' ggplot(my.df, aes(covariate, x)) +
#'   geom_point() +
#'   stat_fit_tb(tb.vars = 1:10) +
#'   expand_limits(y = 70)
#'
#' # Linear regression using a table theme
#' ggplot(my.df, aes(covariate, x)) +
#'   geom_point() +
#'   stat_fit_tb(tb.vars = 7:10) +
#'   expand_limits(y = 70)
#'
#' # Linear regression using a table theme
#' ggplot(my.df, aes(covariate, x)) +
#'   geom_point() +
#'   stat_fit_tb(tb.vars = c(a = 1, b = 2, z = 7)) +
#'   expand_limits(y = 70)
#'
#' # Linear regression using a table theme
#' ggplot(my.df, aes(covariate, x)) +
#'   geom_point() +
#'   stat_fit_tb(tb.vars = c(a = "term", b = "esti", zz = "z")) +
#'   expand_limits(y = 70)
#'
#' # Linear regression using a table theme
#' ggplot(my.df, aes(covariate, x)) +
#'   geom_point() +
#'   stat_fit_tb(tb.vars = c(zz = "z")) +
#'   expand_limits(y = 70)
#'
