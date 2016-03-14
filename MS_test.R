# Matt's testing scripts for wundr
matt.wu.key <- "fd9858dfc94a85ea"
matt.cdb.key <- "f09ad502b34fa4096a62ea306b4650337d41009c"
matt.cdb.account <- "biglakedata"

# get weather data
# dm_cond <- PWS.Conditions("Des Moines, IA", radius=50, user.key=matt.wu.key)
# devtools::use_data(dm_cond, overwrite = T)
data("dm_cond")
data("PWS.Conds.Chicago")

# PWS.Conds.Chicago <- readRDS("data/PWS.Conds.Chicago.rds")
pizza2 <- PWS.Conds.Chicago
r2cdb(matt.cdb.key, matt.cdb.account, pizza2)

# simple plots
simple_pnts(PWS.Conds.Chicago, "Hello World!")
simple_density(PWS.Conds.Chicago, "Hello World!")

# static map
basemap <- set_basemap(PWS.Conds.Chicago, zoom = 12)
gg_points(PWS.Conds.Chicago, basemap)

# interactive web maps
webmap_pnts(PWS.Conds.Chicago)

data("PWS.Conds.Chicago")
webmap_raster(PWS.Conds.Chicago)


# CartoDB

cdbTable <- get_cdb_table("condTest", matt.cdb.account)
head(cdbTable$rows)


matt.cdb.key <- "f09ad502b34fa4096a62ea306b4650337d41009c"
matt.cdb.account <- "biglakedata"
data("PWS.Conds.Chicago")
pizza <- PWS.Conds.Chicago
r2cdb(matt.cdb.key, matt.cdb.account, pizza)


# low-level plots for Stefan computation





## prototype graphical interface
rio <- toSPntsDF(Rio_metadata[[1]])
plot(sp::coordinates(rio))
temp_id <- identify(sp::coordinates(rio))
sp::plot(sp::coordinates(rio)[temp_id, ], col="red", pch = 20, add = TRUE)
# why does the second plot fail to add?
row.names(rio)[temp_id]
# not exactly correct

#
# # methods from Bivand ASDA pp. 76-78
#   # identify point(s) of SpatialPointsDF
# plot(mke.spdf) # basic data.frame works too #this should be points
# temp_id <- identify(coordinates(mke.spdf))
#   # select points on graphics device and click finish (Esc)
# plot(mke.spdf[temp_id, ], col="red", pch = 20, add = TRUE)
# row.names(mke.spdf)[temp_id]






# imports
use_package(package = "sp")
use_package(package = "raster")
use_package(package = "spatstat")
use_package(package = "jsonlite")
use_package(package = "geosphere")
use_package(package = "ggmap")
use_package(package = "leaflet")
use_package(package = "raster")
use_package(package = "RSQLite")
use_package(package = "ggplot2")

# suggests




## ---- other function prototypes -------------------------------------------------------

# simple_poly <- function(spatialPolyDF, title = NULL, add = FALSE, ...){
#   plot(poly, add=add, col="transparent", border="blue", cex.main=.7)
# }

# gg_poly <- function(S4poly, basemap = basemap, title = NULL, ...) {
#   poly <- spTransform(S4poly, WGS84)
#   poly@data$id = rownames(poly@data)
#   poly.points = fortify(poly, region = "id")
#   poly.df = join(poly.points, poly@data, by = "id")
#
#   ggmap(basemap, extent = "device") +
#     geom_polygon(aes(x = long, y = lat, group = group), data = poly.df,
#                  colour = 'blue', fill = 'transparent', alpha = .6, size = 3)
# }

# # add shape boundary (SpatialPolygonDF) to base map
# webmap_poly <- function(SpatialPolygonDF) {
#   poly.WGS84 <- toWGS84(SpatialPolygonDF)
#   bounds     <- poly.WGS84@bbox  # TODO, transform this into proper object for fitBounds(), #alt bbox(bid.poly)
#   m <- leaflet(poly.WGS84)  %>%
#     addProviderTiles("Stamen.TonerLines",options = providerTileOptions(opacity = 0.35)) %>%
#     fitBounds(bounds[1,1],  bounds[2,1], bounds[1,2],  bounds[2,2]) %>%
#     addPolygons(stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5)
#   m
# }

# # create simple raster of point density
# points2raster <- function(SpatialPointsDF){
#   ppp <- as.ppp(SpatialPointsDF)
#   D <- density(ppp)
#   D <- as(D, "RasterLayer")
#   crs(D) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#   D
# }


## --- constants ---------------------------------------------------------------
# A local projection (Milwaukee, Wis.) for spatial calculations
#' NAD27 <- CRS("+proj=lcc +lat_1=42.73333333333333 +lat_2=44.06666666666667
#'              +lat_0=42 +lon_0=-90 +x_0=609601.2192024384 +y_0=0 +datum=NAD27
#'              +units=us-ft +no_defs +ellps=clrk66 +nadgrids=@conus,
#'              @alaska,@ntv2_0.gsb,@ntv1_can.dat")
#'
#' # Web Mercator projection for web mapping
#' WGS84 <- CRS("+proj=longlat +datum=WGS84")
#'
#' ## --- misc helpers ------------------------------------------------------------
#'
#' # transform CRS to Web Merator for web mapping
#' toWGS84 <- function(sp) {
#'   WGS84 <- CRS("+proj=longlat +datum=WGS84")
#'   spTransform(sp, WGS84)
#' }
