# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This file contains Matthew Schumwinger's (mjs13) code.
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
#' }
draw_subset <- function(PWS.class){
  message("select points on graphics device and click finish (Esc)")
  spdf <- PWS.class@spatialPtDF
  sp::plot(spdf)
  poly <- locator(type = "o")
  n <- length(poly$x)
  p <- sp::Polygon(coords = cbind(poly$x, poly$y)[c(1:n, 1), ], hole = FALSE)
  ps <- sp::Polygons(srl = list(p), ID = "poly")
  sps <- sp::SpatialPolygons(Srl = list(ps))
  sp::plot(spdf[sps, ], col="red", pch = 20, add = TRUE)
  cat("IDs of PWS in subset:\n")
  print(spdf[sps, ]@data$id)
  PWS.class@spatialPtDF <- spdf[sps, ] # subset spdf
  # note: currently, this does not subsset the conditions data slot
  PWS.class
}


#' milw_metadata dataset
#'
#' This contains Personal Weather Stations meta data for the Milwaukee, Wis.
#' area.
#'
#' @examples
#' data(milw_metadata)
#'
#' @author wundr team
"milw_metadata"


#' milw_conds dataset
#'
#' This contains Personal Weather Stations conditions data for the Milwaukee,
#' Wis. PWS.
#'
#' @examples
#' data(milw_conds)
#'
#' @author wundr team
"milw_conds"
