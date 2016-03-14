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
gg_points(PWS.Conds.Chicago, basemap, title = "Pizza")
ggmap::gglocator()

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
pizza2 <- PWS.Conds.Chicago
r2cdb(matt.cdb.key, matt.cdb.account, pizza2)


# graphical subsetting
my_ss  <-  draw_subset(PWS.Conds.Chicago)
my_ss2 <-  draw_subset(my_ss)





## prototype graphical interface -----------
## simple plot
spdf <- PWS.Conds.Chicago@spatialPtDF

  # identify point(s) of SpatialPointsDF
sp::plot(spdf)
locator()

# select subset of points

sel_id <- identify(sp::coordinates(spdf))
  # select points on graphics device and click finish (Esc)
sp::plot(spdf[sel_id, ], col="red", pch = 20, add = TRUE)
row.names(spdf)[sel_id]
spdf@data[sel_id, "id"]
spdf@coords[sel_id, ]

# draw polygon to subset # methods from Bivand ASDA pp. 76-78
sp::plot(spdf)
poly <- locator(type = "o")
n <- length(poly$x)
p <- sp::Polygon(cbind(poly$x, poly$y)[c(1:n, 1), ], hole = FALSE)
ps <- sp::Polygons(list(p), ID = "poly")
sps <- sp::SpatialPolygons(list(ps))
sp::plot(spdf[sps, ], col="red", pch = 20, add = TRUE)
# select points on graphics device and click finish (Esc)
spdf[sps, ] # subset spdf
print(spdf[sps, ]@data$id)



## static
basemap <- set_basemap(PWS.Conds.Chicago, zoom = 12)
gg_points(PWS.Conds.Chicago, basemap)
ggmap::gglocator()

-----------



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
NAD27 <- CRS("+proj=lcc +lat_1=42.73333333333333 +lat_2=44.06666666666667
             +lat_0=42 +lon_0=-90 +x_0=609601.2192024384 +y_0=0 +datum=NAD27
             +units=us-ft +no_defs +ellps=clrk66 +nadgrids=@conus,
             @alaska,@ntv2_0.gsb,@ntv1_can.dat")

# Web Mercator projection for web mapping
WGS84 <- sp::CRS("+proj=longlat +datum=WGS84")

## --- misc helpers ------------------------------------------------------------

# transform CRS to Web Merator for web mapping
toWGS84 <- function(sp) {
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84")
  sp::spTransform(sp, WGS84)
}
