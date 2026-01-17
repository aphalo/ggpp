utils::globalVariables(c(
  "from_theme",
  "colour",
  "ink",
  "family",
  "fill",
  "paper",
  "fontsize",
  "borderwidth",
  "bordertype",
  "linewidth",
  "linetype",
  "pointshape",
  "pointsize"
))

.onLoad <- function(libname, pkgname) {

  # if 'ggplot2' >= 4.0.0 we update the Geom "definitions" to retrieve defaults
  # from the theme, as the geom element is available
  if ("element_geom" %in% getNamespaceExports("ggplot2")) {
    ggplot2::update_geom_defaults(
      GeomGrob,
      ggplot2::aes(colour = from_theme(colour %||% ink),
                   family = from_theme(family)))
    ggplot2::update_geom_defaults(
      GeomGrobNpc,
      ggplot2::aes(colour = from_theme(colour %||% ink),
                   family = from_theme(family)))
    ggplot2::update_geom_defaults(
        GeomLabelS,
        ggplot2::aes(colour = from_theme(colour %||% ink),
                     fill = from_theme(fill %||% paper),
                     family = from_theme(family),
                     size = from_theme(fontsize),
                     linewidth = from_theme(borderwidth * 0.5),
                     linetype  = from_theme(bordertype)))
      ggplot2::update_geom_defaults(
        GeomLabelNpc,
        ggplot2::aes(colour = from_theme(colour %||% ink),
                     fill = from_theme(fill %||% paper),
                     family = from_theme(family),
                     size = from_theme(fontsize),
                     linewidth = from_theme(borderwidth * 0.5),
                     linetype  = from_theme(bordertype)))
      ggplot2::update_geom_defaults(
        GeomLabelPairwise,
        ggplot2::aes(colour = from_theme(colour %||% ink),
                     fill = from_theme(fill %||% paper),
                     family = from_theme(family),
                     size = from_theme(fontsize),
                     linewidth = from_theme(borderwidth * 0.5),
                     linetype  = from_theme(bordertype)))
      ggplot2::update_geom_defaults(
        GeomTextS,
        ggplot2::aes(colour = from_theme(colour %||% ink),
                     family = from_theme(family),
                     size = from_theme(fontsize)))
      ggplot2::update_geom_defaults(
        GeomTextNpc,
        ggplot2::aes(colour = from_theme(colour %||% ink),
                     family = from_theme(family),
                     size = from_theme(fontsize)))
      ggplot2::update_geom_defaults(
        GeomTextPairwise,
        ggplot2::aes(colour = from_theme(colour %||% ink),
                     family = from_theme(family),
                     size = from_theme(fontsize)))
      ggplot2::update_geom_defaults(
        GeomPointS,
        ggplot2::aes(shape = from_theme(pointshape),
                     colour = from_theme(colour %||% ink),
                     fill = from_theme(fill %||% NA),
                     size = from_theme(pointsize),
                     stroke = from_theme(borderwidth)))
      ggplot2::update_geom_defaults(
        GeomXMarginPoint,
        ggplot2::aes(shape = from_theme(pointshape),
                     colour = from_theme(colour %||% ink),
                     fill = from_theme(fill %||% NA),
                     size = from_theme(pointsize),
                     stroke = from_theme(borderwidth)))
      ggplot2::update_geom_defaults(
        GeomYMarginPoint,
        ggplot2::aes(shape = from_theme(pointshape),
                     colour = from_theme(colour %||% ink),
                     fill = from_theme(fill %||% NA),
                     size = from_theme(pointsize),
                     stroke = from_theme(borderwidth)))
      ggplot2::update_geom_defaults(
        GeomXMarginArrow,
        ggplot2::aes(colour = from_theme(colour %||% ink),
                     fill = from_theme(fill %||% paper),
                     linewidth = from_theme(linewidth)))
      ggplot2::update_geom_defaults(
        GeomYMarginArrow,
        ggplot2::aes(colour = from_theme(colour %||% ink),
                     fill = from_theme(fill %||% paper),
                     linewidth = from_theme(linewidth)))
      ggplot2::update_geom_defaults(
        GeomXMarginGrob,
        ggplot2::aes(colour = from_theme(colour %||% ink),
                     family = from_theme(family)))
      ggplot2::update_geom_defaults(
        GeomYMarginGrob,
        ggplot2::aes(colour = from_theme(colour %||% ink),
                     family = from_theme(family)))
      ggplot2::update_geom_defaults(
        GeomPlot,
        ggplot2::aes(colour = from_theme(colour %||% ink),
                     family = from_theme(family)))
      ggplot2::update_geom_defaults(
        GeomPlotNpc,
        ggplot2::aes(colour = from_theme(colour %||% ink),
                     family = from_theme(family)))
      ggplot2::update_geom_defaults(
        GeomTable,
        ggplot2::aes(family = from_theme(family),
                     size = from_theme(fontsize)))
      ggplot2::update_geom_defaults(
        GeomTableNpc,
        ggplot2::aes(family = from_theme(family),
                     size = from_theme(fontsize)))
      ggplot2::update_geom_defaults(
        GeomQuadrantLines,
        ggplot2::aes(colour = from_theme(colour %||% ink),
                     linewidth = from_theme(linewidth)))
      ggplot2::update_geom_defaults(
        GeomVHLines,
        ggplot2::aes(colour = from_theme(colour %||% ink),
                     linewidth = from_theme(linewidth)))
    }

}
