# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Yada...yada
# The primary functions include:
#
#  o
#  o
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# library(spatstat) #require for heatmap
# library(raster) #require for heatmap
# library(leaflet) # require for webmap
# library(htmltools) # require for webmap

# library(rgdal)
# library(maptools)
# library(plyr)
# library(ggplot2)
# library(dplyr)
# library(geoR)
# library(sm)
# library(pander)
# library(colorRamps)
# library(scales)
# library(RColorBrewer)
# library(ggmap)
# library(geosphere)
# library(maptools)
# library(sp)

# --- Functions for simple plots using package sp ------------------------------

#' toSPntsDF
#'
#' Transform a data frame of PWS meta data to a spatialPointsDataFrame.
#'
#' @importFrom sp spTransform
#' @importFrom sp CRS
#' @importFrom sp SpatialPointsDataFrame
#' @param df A data frame of PWS meta data.
#' @return A spatialPointsDataFrame.
#' @export
#' @examples
#' toSPntsDF(Rio_metadata)
toSPntsDF <- function(df){
  # transform CRS to Web Merator for web mapping
  toWGS84 <- function(spatialPtDF) {
    WGS84 <- CRS("+proj=longlat +datum=WGS84")
    spTransform(sp, WGS84)
  }
  df_mat <- cbind(df$lon, df$lat)
  row.names(df_mat) <- row.names(df)
  SpatialPointsDataFrame(df_mat, df, proj4string = WGS84, match.ID = TRUE)
}


#' simple_pnts
#'
#' A simple 2D plot of spatial points.
#' @param spatialPtDF A SpatialPointsDataFrame.
#' @param title A title for you plot. Default = NULL.
#' @return NULL
#' @export
#' @examples
#' Rio_spdf <- toWGS84(Rio_metadata)
#' simple_pnts(Rio_spdf, "Rio PWS Locations")

simple_pnts <- function(spatialPtDF, title = NULL, add = FALSE, ...){
  plot(spatialPtDF, add=add, col="red", cex=.5, pch =20, main = title)
}

simple_poly <- function(spatialPolyDF, title = NULL, add = FALSE, ...){
  plot(poly, add=add, col="transparent", border="blue", cex.main=.7)
}

# --- Function for simple density raster using pacakges spatstat and raster ----
# BUG: this breaks b/c S4 has no coordinate system
heatmap <- function(spatialPtDF, title = NULL, add = FALSE, ...){
  ppp <- as.ppp(spatialPtDF)
  D <- density(ppp)
  D <- as(D, "RasterLayer")
  mycol <- colorRampPalette(c("transparent", "transparent","yellow", "orange","red"))(256)
  plot(D, legend = F, box = F, axes = F, col=mycol, add=F, main = title)
  # box()
  contour(D, axes = FALSE, add=T, col = "white", drawlabels=F)
}

# --- functions for static plots on contextual map backgrounds using Hadley's package ggplot and dplyr...
  # following Hadly's ggmap paper
  # set contextual basemap based on data's center
  # TODO: set to data's extent/bounds with make_bbox(lon, lat, data, f = 0.05)
  # TODO: allow for layering one map layer upon another
  set_basemap <- function(mapdata, location = NULL, zoom = 10) {
    center <- c(-87.896118, 42.946622) # MKE airport
    # TODO make function to calculate centroid from bbox of data
    #   t.mapdata <- spTransform(mapdata, WGS84)
    #   c.mapdata <- rowMeans(bbox(t.mapdata))
    basemap <- get_map(location = center, zoom = zoom,
                       maptype = "toner-lite", source = "stamen")
  }

# temporary hack version of set_basemap
# TODO: merge into above with option to name location with string
set_basemap2 <- function(mapdata, location = NULL, zoom = 10) {
  basemap <- get_map(location = location, zoom = zoom,
                     maptype = "toner-lite", source = "stamen")
}

gg_poly <- function(S4poly, basemap = basemap, title = NULL, ...) {
  poly <- spTransform(S4poly, WGS84)
  poly@data$id = rownames(poly@data)
  poly.points = fortify(poly, region = "id")
  poly.df = join(poly.points, poly@data, by = "id")

  ggmap(basemap, extent = "device") +
    geom_polygon(aes(x = long, y = lat, group = group), data = poly.df,
                 colour = 'blue', fill = 'transparent', alpha = .6, size = 3)
}

# BUG: because loaded into S4?
gg_points <- function(S4pnts, basemap = basemap, title = NULL, ...) {
  # poly <- spTransform(pnts, WGS84)
  pnts <- S4pnts@spatialPtDF
  ggmap(basemap, extent = "device") +
    geom_point(data=as.data.frame(pnts), aes(coords.x1,coords.x2), col= "red", alpha =.8)
}

# --- functions for web mapping ------------------------------------------------
#   deploy data to leaflet.js using rstudio's package::leaflet and widget system.
#   devtools::install_github("rstudio/leaflet")

# add shape boundary (SpatialPolygonDF) to base map
webmap_poly <- function(SpatialPolygonDF) {
  poly.WGS84 <- toWGS84(SpatialPolygonDF)
  bounds     <- poly.WGS84@bbox  # TODO, transform this into proper object for fitBounds(), #alt bbox(bid.poly)
  m <- leaflet(poly.WGS84)  %>%
    addProviderTiles("Stamen.TonerLines",options = providerTileOptions(opacity = 0.35)) %>%
    fitBounds(bounds[1,1],  bounds[2,1], bounds[1,2],  bounds[2,2]) %>%
    addPolygons(stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5)
  m
}

# add points (SpatialPointsDF) to base map
webmap_pnts <- function(S4pnts) {
  # pnts.WGS84 <- toWGS84(SpatialPointsDF)
  SpatialPointsDF <- S4pnts@spatialPtDF
  bounds <- SpatialPointsDF@bbox
  m <- leaflet(SpatialPointsDF)  %>%
    addProviderTiles("Stamen.TonerLines",options = providerTileOptions(opacity = 0.35)) %>%
    fitBounds(bounds[1,1],  bounds[2,1], bounds[1,2],  bounds[2,2]) %>%
    # addCircles(color="red")
    addCircles(color="red", popup = ~htmlEscape(station_id)) # TODO parameterize popupColumns
  m
}

# create simple raster of point density
points2raster <- function(SpatialPointsDF){
  ppp <- as.ppp(SpatialPointsDF)
  D <- density(ppp)
  D <- as(D, "RasterLayer")
  crs(D) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  D
}

# plot raster image of points
# https://rstudio.github.io/leaflet/raster.html
webmap_points2raster <- function(SpatialPointsDF){
  D <- points2raster(SpatialPointsDF)
  pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(D),
                      na.color = "transparent")
  bounds <- SpatialPointsDF@bbox
  d <- leaflet(SpatialPointsDF)  %>%
    addProviderTiles("Stamen.TonerLines",options = providerTileOptions(opacity = 0.35)) %>%
    fitBounds(bounds[1,1],  bounds[2,1], bounds[1,2],  bounds[2,2]) %>%
    addRasterImage(D, colors = pal, opacity = 0.8)
  d
}


# --- constants ----------------------------------------------------------------
# A local projection (Milwaukee, Wis.) for spatial calculations
NAD27 <- CRS("+proj=lcc +lat_1=42.73333333333333 +lat_2=44.06666666666667
             +lat_0=42 +lon_0=-90 +x_0=609601.2192024384 +y_0=0 +datum=NAD27
             +units=us-ft +no_defs +ellps=clrk66 +nadgrids=@conus,
             @alaska,@ntv2_0.gsb,@ntv1_can.dat")

# Web Mercator projection for web mapping
WGS84 <- CRS("+proj=longlat +datum=WGS84")

# --- misc helpers -------------------------------------------------------------
# transform data.frame to spatialPointsDataFrame

# transform CRS to Web Merator for web mapping
# TODO: add ability to transform rasterlayers and use with points2raster()?
# BUG: this breaks b/c S4 has no coordinate system
toWGS84 <- function(spatialPtDF) {
  WGS84 <- CRS("+proj=longlat +datum=WGS84")
  spTransform(sp, WGS84)
}

