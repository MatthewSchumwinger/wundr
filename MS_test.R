# Matt's testing scripts for wundr
matt.wu.key <- "fd9858dfc94a85ea"
matt.cdb.key <- "f09ad502b34fa4096a62ea306b4650337d41009c"
matt.cdb.account <- "biglakedata"

# get weather data
# dm_cond <- PWS.Conditions("Des Moines, IA", radius=50, user.key=matt.wu.key)
# devtools::use_data(dm_cond, overwrite = T)
data("dm_cond")

# simple plots
simple_pnts(dm_cond, "Hello World!")
simple_density(dm_cond, "Hello World!")

# static map
basemapDM <- set_basemap(dm_cond, zoom = 12)
gg_points(dm_cond, basemapDM)

# interactive web maps
webmap_pnts(dm_cond)
webmap_raster(dm_cond)

# CartoDB
cdbTable <- get_cdb_table("condTest", matt.cdb.account)
head(cdbTable$rows)

r2cdb(matt.cdb.key, matt.cdb.account, dm_cond)


# low-level plots for Stefan computation





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
