##
## Begin Matthew Schumwinger's (mjs13) code
##


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This file contains Matthew Schumwinger's (mjs13) code.
#
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
#' data(Rio_metadata)
#' toSPntsDF(Rio_metadata$PWSmetadata)
toSPntsDF <- function(df){
  # ESPG:WGS84 is Web Mercator projection for web mapping
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84")
  df_mat <- cbind(df$lon, df$lat)
  row.names(df_mat) <- row.names(df)
  sp::SpatialPointsDataFrame(coords = df_mat, data = df, proj4string = WGS84,
                             match.ID = TRUE)
}

#' simple_pnts
#'
#' A simple 2D plot of spatial points.
#'
#' @param PWS.class A PWS.class points S4 object.
#' @param title character string. A title for you plot. Default = NULL.
#' @param add logical. Default = FALSE. If TRUE, add to already existing plot.
#' @param col character. Color of points. Default = "red".
#' @param ... ...
#' @return NULL
#' @export
#' @examples
#' data("PWS.Conds.Chicago")
#' simple_pnts(PWS.Conds.Chicago, "Hello World!")
simple_pnts <- function(PWS.class, title = NULL, add = FALSE, col = "red", ...){
  spatialPtDF <- PWS.class@spatialPtDF
  plot(spatialPtDF, add = add, cex = .5, pch = 20, main = title)
}

#' simple_density
#'
#' Creates a simple density raster plot (heatmap) of PWS locations.
#' @importFrom spatstat as.ppp
#' @importFrom raster density contour
#' @param PWS.class A PWS.class points S4 object.
#' @param title A title for you plot. Default = NULL.
#' @param ... ...
#' @param add Whether to add this "on top" of previous plot. Default = NULL.
#' @return A RasterLayer.
#' @export
#' @examples
#' data("PWS.Conds.Chicago")
#' simple_density(PWS.Conds.Chicago, title =
#'   "Chicago PWS points over \nPWS density contour")
#' simple_pnts(PWS.Conds.Chicago,
#'   "Chicago PWS points over \nPWS density contour", add = TRUE)
simple_density <- function(PWS.class, title = NULL, add = FALSE, ...){
  df <- PWS.class@spatialPtDF@data
  ppp <- spatstat::ppp(x = df$lon, y = df$lat, xrange = range(df$lon),
                       yrange = range(df$lat))
  D <- raster::density(ppp)
  D <- as(D, "RasterLayer")
  mycol <- grDevices::colorRampPalette(colors = c("transparent", "transparent", "yellow",
                                       "orange", "red"))(256)
  plot(D, legend = FALSE, box = FALSE, axes = FALSE, col = mycol, add = FALSE,
       main = title)
  raster::contour(D, axes = FALSE, add = TRUE, col = "white",
                  drawlabels = FALSE)
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
#' data("PWS.Conds.Chicago")
#' basemap <- set_basemap(PWS.Conds.Chicago)
set_basemap <- function(PWS.class, zoom = 9) {
  message("Note: zoom = 9 captures 50-mile radius.")
  df <- PWS.class@spatialPtDF@data
  bbox <- ggmap::make_bbox(lon = df$lon, lat = df$lat)
  center <- c(mean(c(bbox[1], bbox[3])), mean(c(bbox[2], bbox[4])))
  basemap <- ggmap::get_map(location = center, zoom = zoom,
                     maptype = "toner-lite", source = "stamen")
}

#' gg_points
#'
#' Plots PWS locations on a contextual basemap.
#'
#' @importFrom ggmap ggmap
#' @importFrom ggplot2 geom_point aes_string
#' @param PWS.class A PWS.class points S4 object.
#' @param basemap A contextual basemap. See set_basemap.
#' @param title A character string title for the map. Default = NULL.
#' @param ... ...
#' @return NULL
#' @export
#' @examples
#' data("PWS.Conds.Chicago")
#' basemap <- set_basemap(PWS.Conds.Chicago, zoom = 12)
#' gg_points(PWS.Conds.Chicago, basemap, title = "Downtown Chicago PWS")
gg_points <- function(PWS.class, basemap = basemap, title = NULL, ...) {
  message(c("Data points outside zoom area are considered 'missing values'",
            "\nand may not plot on gg_map if zoom > 9."))
  pnts <- PWS.class@spatialPtDF@data
  ggmap::ggmap(basemap, extent = "device") +
    ggplot2::geom_point(data = pnts,
                        ggplot2::aes_string(x = "lon", y = "lat"),
                        col = "red", alpha =.8) +
    ggplot2::ggtitle(title)
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
#' data(PWS.Conds.Chicago)
#' webmap_pnts(PWS.Conds.Chicago)
webmap_pnts <- function(PWS.class, content = content) {
  metadata <- PWS.class@spatialPtDF
  data <- PWS.class@data
  bounds <- metadata@bbox
  content <- paste(sep = "",
                     "<b>ID: ", data$station_id,"</b>", "<br/>",
                     data$temperature_string, "<br/>",
                     data$weather, "<br/>",
                     data$observation_time)
  m <- leaflet::leaflet(metadata)  %>%
    leaflet::addProviderTiles("Stamen.TonerLines",
                     options = leaflet::providerTileOptions(opacity = 0.35)) %>%
    leaflet::fitBounds(bounds[1,1], bounds[2,1], bounds[1,2], bounds[2,2]) %>%
    leaflet::addCircles(color = "red", popup = content)
  m
}


#' webmap_raster
#'
#' Interactive web map of PWS stations density (heatmap).
#'
#' @importFrom spatstat ppp
#' @importFrom raster density crs values
#' @importFrom sp CRS
#' @importFrom leaflet leaflet addProviderTiles %>% providerTileOptions
#'   colorNumeric addRasterImage
#' @param PWS.class A PWS.class points S4 object.
#' @return NULL
#' @export
#' @examples
#' \dontrun{
#' data(PWS.Conds.Chicago)
#' webmap_raster(PWS.Conds.Chicago)
#' }
webmap_raster <- function(PWS.class){
  spdf <- toSPntsDF(PWS.class@spatialPtDF@data)
  ppp <- spatstat::ppp(x = spdf$lon, y = spdf$lat, xrange= range(spdf$lon),
                       yrange = range(spdf$lat))
  D <- raster::density(ppp)
  D <- as(D, "RasterLayer")
  D@crs <- sp::CRS("+proj=longlat +datum=WGS84") # projection for web mapping
  pal <- leaflet::colorNumeric(palette = c("transparent", "#41B6C4", "#FFFFCC"),
                               domain = raster::values(D), na.color = "transparent",
                               alpha = TRUE)
  d = leaflet::leaflet(spdf)  %>%
    leaflet::addProviderTiles("Stamen.TonerLines", options =
                                leaflet::providerTileOptions(opacity = 0.35)) %>%
    leaflet::addRasterImage(D, colors = pal, opacity = 0.8)

  d
}

##
## Begin Matthew Schumwinger's (mjs13) code
##
