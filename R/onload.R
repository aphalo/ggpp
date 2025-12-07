.onLoad <- function(libname, pkgname) {

  if (utils::packageVersion("ggplot2") < "4.0.0") {
    from_theme <<- function(x) {
      switch(x,
             ink        = "black",
             paper      = "white",
             accent     = "#3366FF",
             linewidth  = 0.5,
             borderwidth= 0.5,
             linetype   = 1,
             bordertype = 1,
             family     = "",
             fontsize   = 3.87,
             pointsize  = 1.5,
             pointshape = 19,
             colour     = NULL,
             fill       = NULL,
             NULL)
    }
  } else {
    from_theme <<- ggplot2::from_theme
  }

}
