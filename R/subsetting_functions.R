#' draw_subset
#'
#' Graphicly subset PWS.class objects by drawing a poylgon around PWS points
#' plotted in graphics device. Adapted from Roger Bivand's Applied Spatial Data
#' Analysis with R (pp. 76-78).
#'
#' @importFrom sp plot Polygon Polygons SpatialPolygons
#' @param PWS.class A PWS.class points S4 object.
#' @return A PWS.class points S4 object.
#' @export
#' @examples
#' \dontrun{
#' # not run because this is an function that requires user interaction.
#' my_ss  <-  draw_subset(PWS.Conds.Chicago)
#' my_ss2 <-  draw_subset(my_ss)}

# draw polygon to subset # methods from Bivand ASDA pp. 76-78
draw_subset <- function(PWS.class){
  cat("select points on graphics device and click finish (Esc)")
  spdf <- PWS.class@spatialPtDF
  sp::plot(spdf)
  poly <- locator(type = "o")
  n <- length(poly$x)
  p <- sp::Polygon(cbind(poly$x, poly$y)[c(1:n, 1), ], hole = FALSE)
  ps <- sp::Polygons(list(p), ID = "poly")
  sps <- sp::SpatialPolygons(list(ps))
  sp::plot(spdf[sps, ], col="red", pch = 20, add = TRUE)
  cat("Please select points on graphics device and click finish (Esc).\n")
  cat("IDs of PWS in subset:\n")
  print(spdf[sps, ]@data$id)
  PWS.class@spatialPtDF <- spdf[sps, ] # subset spdf
  PWS.class
}
