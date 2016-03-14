# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This document contains functions that visualize Weatherunderground personal
# weather station (PWS) locations and data. Functions that allow the R user to
# make exploratory and explanitory plots and maps are provided.
# The primary functions include:
#
#  o simple_density
#  o set_basemap
#  o gg_points
#  o webmap_pnts
#  o webmap_raster
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' toSPntsDF
#'
#' Transform a simple data frame of PWS meta data to a spatialPointsDataFrame.
#'
#' @importFrom sp spTransform CRS SpatialPointsDataFrame
#' @param df A data frame of PWS meta data.
#' @return A spatialPointsDataFrame.
#' @export
#' @examples
#' toSPntsDF(Rio_metadata)
toSPntsDF <- function(df){
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84")
  WGS84
  # transform CRS to Web Merator for web mapping
  toWGS84 <- function(spatialPtDF) {
    sp::spTransform(sp, WGS84)
  }
  df_mat <- cbind(df$lon, df$lat)
  row.names(df_mat) <- row.names(df)
  sp::SpatialPointsDataFrame(df_mat, df, proj4string = WGS84, match.ID = TRUE)
}

#' simple_pnts
#'
#' A simple 2D plot of spatial points.
#'
#' @param PWS.class A PWS.class points S4 object.
#' @param title A title for you plot. Default = NULL.
#' @return NULL
#' @export
#' @examples
#' data(dm_cond)
#' simple_pnts(dm_cond, "Des Moines, IA PWS Locations")
simple_pnts <- function(PWS.class, title = NULL, add = FALSE, ...){
  spatialPtDF <- PWS.class@spatialPtDF
  plot(spatialPtDF, add=add, col="red", cex=.5, pch =20, main = title)
}

#' simple_density
#'
#' Creates a simple density raster plot (heatmap) of PWS locations.
#' @importFrom spatstat as.ppp
#' @importFrom raster density contour
#' @param PWS.class A PWS.class points S4 object.
#' @param title A title for you plot. Default = NULL.
#' @param add Whether to add this "on top" of previous plot. Default = NULL.
#' @return A RasterLayer.
#' @export
#' @examples
#' data(dm_cond)
#' simple_density(dm_cond, "Des Moines, IA PWS Locations")
simple_density <- function(PWS.class, title = NULL, add = FALSE, ...){
  df <- PWS.class@spatialPtDF@data
  ppp <- spatstat::ppp(df$lon, df$lat, range(df$lon), range(df$lat))
  D <- raster::density(ppp)
  D <- as(D, "RasterLayer")
  mycol <- colorRampPalette(c("transparent", "transparent","yellow", "orange","red"))(256)
  plot(D, legend = F, box = F, axes = F, col=mycol, add=F, main = title)
  # box()
  raster::contour(D, axes = FALSE, add=T, col = "white", drawlabels=F)
  D
}

#' set_basemap
#'
#' Creates subtle contextual map background for ggplots based on extent points.
#' @importFrom ggmap make_bbox get_map
#' @param PWS.class A PWS.class points S4 object.
#' @param zoom A zoom level for the contextual map. Greater values result in
#'   increased zoom. Default = 9, which captures a 50-mile radius.
#' @return A ggmap raster object.
#' @export
#' @examples
#' data(dm_cond)
#' set_basemap(dm_cond)
set_basemap <- function(PWS.class, zoom = 9) {
  cat("Note: zoom = 9 captures 50-mile radius.")
  df <- PWS.class@spatialPtDF@data
  bbox <- ggmap::make_bbox(df$lon, df$lat)
  # center <- c(-87.896118, 42.946622) # MKE airport
  center <- c(mean(c(bbox[1], bbox[3])), mean(c(bbox[2], bbox[4])))
  basemap <- ggmap::get_map(location = center, zoom = zoom,
                     maptype = "toner-lite", source = "stamen")
}

#' gg_points
#'
#' Plots PWS locations on a contextual basemap.
#'
#' @importFrom ggmap ggmap
#' @importFrom ggplot2 geom_point aes
#' @param PWS.class A PWS.class points S4 object.
#' @param basemap A contextual basemap. See set_basemap.
#' @return NULL
#' @export
#' @examples
#' data(dm_cond)
#' basemapDM <- set_basemap(dm_cond, zoom = 9)
#' gg_points(dm_cond)
gg_points <- function(PWS.class, basemap = basemap, title = NULL, ...) {
  cat("Note: zoom = 9 captures 50-mile radius.", "\n",
      "Data points outside zoom area are considered 'missing values'", "\n",
      "and may not plot on gg_map if zoom > 9")
  pnts <- PWS.class@spatialPtDF
  ggmap::ggmap(basemap, extent = "device") +
    ggplot2::geom_point(data=as.data.frame(pnts),
                        ggplot2::aes(coords.x1,coords.x2), col= "red",alpha =.8)
}

#' webmap_pnts
#'
#' Interactive web map of PWS stations.
#'
#' @importFrom leaflet leaflet addProviderTiles fitBounds addCircles %>%
#'   providerTileOptions
#' @param PWS.class A PWS.class points S4 object.
#' @param content A string in html format used to populate PWS pop-up window.
#'   Default displays PWS id, tempurature, and URL to historical data. Condition
#'   data values should be in the followin format: `data$[columndname]'.
#' @return NULL
#' @export
#' @examples
#' data(dm_cond)
#' webmap_pnts(dm_cond)
webmap_pnts <- function(PWS.class, content = content) {
  data <- PWS.class@spatialPtDF
  bounds <- data@bbox
  content <- paste(sep = "",
                     "<b>", data$id,"</b>", "<br/>",
                     data$temperature_string, "<br/>",
                     "<a href=", data$history_url, ">current and historical
                      data</a>")
  m <- leaflet::leaflet(data)  %>%
    leaflet::addProviderTiles("Stamen.TonerLines",
                     options = leaflet::providerTileOptions(opacity = 0.35)) %>%
    leaflet::fitBounds(bounds[1,1], bounds[2,1], bounds[1,2], bounds[2,2]) %>%
    leaflet::addCircles(color="red", popup = content)
  m
}


#' webmap_raster
#'
#' Interactive web map of PWS stations density (heatmap).
#'
#' @importFrom spatstat ppp
#' @importFrom raster density crs
#' @importFrom sp CRS
#' @importFrom leaflet leaflet colorNumeric addProviderTiles addRasterImage %>%
#' @param PWS.class A PWS.class points S4 object.
#' @return NULL
#' @export
#' @examples
#' data(dm_cond)
#' webmap_raster(dm_cond)
webmap_raster <- function(PWS.class){
  spdf <- toSPntsDF(PWS.class@spatialPtDF@data)
  ppp <- spatstat::ppp(spdf$lon, spdf$lat, range(spdf$lon), range(spdf$lat))
  D <- raster::density(ppp)
  D <- as(D, "RasterLayer")
  raster::crs(D) <- sp::CRS("+proj=longlat +datum=WGS84") # projection for web mapping
  pal <- leaflet::colorNumeric(c("transparent", "#41B6C4", "#FFFFCC"), values(D),
                      na.color = "transparent", alpha=TRUE)
  d = leaflet::leaflet(spdf)  %>%
    leaflet::addProviderTiles("Stamen.TonerLines",options =
                              leaflet::providerTileOptions(opacity = 0.35)) %>%
    leaflet::addRasterImage(D, colors = pal, opacity = 0.8)
  d
}




