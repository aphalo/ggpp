% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate.r
\name{annotate}
\alias{annotate}
\title{Annotations supporting NPC}
\usage{
annotate(
  geom,
  x = NULL,
  y = NULL,
  xmin = NULL,
  xmax = NULL,
  ymin = NULL,
  ymax = NULL,
  xend = NULL,
  yend = NULL,
  npcx = NULL,
  npcy = NULL,
  label = NULL,
  ...,
  na.rm = FALSE
)
}
\arguments{
\item{geom}{character Name of geom to use for annotation.}

\item{x, y, xmin, ymin, xmax, ymax, xend, yend, npcx, npcy}{numeric Positioning
aesthetics - you must specify at least one of these.}

\item{label}{character, data.frame, ggplot or grob.}

\item{...}{Other named arguments passed on to \code{layer()}. These are often
aesthetics, used to set an aesthetic to a fixed value, like color = "red"
or size = 3. They may also be parameters to the paired geom/stat.}

\item{na.rm}{logical If \code{FALSE}, the default, missing values are removed
with a warning. If TRUE, missing values are silently removed.}
}
\value{
A plot layer instance.
}
\description{
A revised version of \code{annotate()} from package 'ggplot2' adding support
for \code{npcx} and \code{npcy} position aesthetics, allowing use of the
geometries defined in the current package such as \code{geom_text_npc()}. It
also has a parameter \code{label} that directly accepts data frames, ggplots
and grobs as arguments in addition to objects of atomic classes like
character. When package 'ggpmisc' is loaded this definition of
\code{annotate()} overrides that in package 'ggplot2'.
}
\details{
Note that all position aesthetics are scaled (i.e., they will
  expand the limits of the plot so they are visible), but all other
  aesthetics are set. This means that layers created with this function will
  never affect the legend.
}
\note{
To use the original definition of \code{annotate()} after loading
  package 'ggpmisc', use \code{ggplot2::annotate()}.
}
\examples{

p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()

# Works as ggplot2::annotate()
p + annotate("text", x = 5, y = 32, label = "Some text")
p + annotate("label", x = c(2, 5), y = c(15, 32),
             label = c("A", "B"))
p + annotate("table", x = 5, y = 30,
             label = data.frame(A = 1:2, B = letters[1:2]))
p + annotate("plot", x = 5.5, y = 34,
             label = p + theme_bw(9))
p + annotate("rect", xmin = 3, xmax = 4.2, ymin = 12, ymax = 21, alpha = .2)
p + annotate("segment", x = 2.5, xend = 4, y = 15, yend = 25, colour = "blue")
p + annotate("pointrange", x = 3.5, y = 20, ymin = 12, ymax = 28,
  colour = "red", size = 1.5)

# But ggpmisc::annotate() also works with npcx and npcy pseudo-aesthetics
p + annotate("label_npc", npcx = c(0.1, 0.9), npcy = c(0.1, 0.9),
             label = c("A", "B"))
p + annotate("label_npc", npcx = 0.9, npcy = c(0.1, 0.9),
             label = c("A", "B"))

p + annotate("text_npc", npcx = 0.9, npcy = 0.9, label = "Some text")
p + annotate("text_npc", npcx = "right", npcy = "top", label = "Some text")

p + annotate("table_npc", npcx = 0.9, npcy = 0.9,
             label = data.frame(A = 1:2, B = letters[1:2]))

p + annotate("plot_npc", npcx = 1, npcy = 1,
             label = p + theme_bw(9))
p + annotate("plot_npc", npcx = c(0, 1), npcy = c(0, 1),
             label = list(p + theme_bw(9), p + theme_grey(9)),
             vp.width = 0.3, vp.height = 0.4)

}
