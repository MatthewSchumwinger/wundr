# Matt's testing scripts for wundr
matt.wu.key <- "fd9858dfc94a85ea"
matt.cdb.key <- "f09ad502b34fa4096a62ea306b4650337d41009c"
matt.cdb.account <- "biglakedata"


#################### prototype #################################################

#' click_meta_query
#'
#' Graphicly select new geographic location and run PSW_meta_query.
#'
#' @importFrom ggmap make_bbox get_map gglocator
#' @importFrom ggplot2 geom_point aes_string
#' @param PWS.Conditions A PWS.Conditions points S4 object.
#' @return A PWS.Locations S4 object.
#' @export
#' @examples
#' \dontrun{
#' ## TODO this doesn't work because unexpected object type returned by PWS_meta_query
#' milw_metadata <- click_meta_query(PWS.Conds.Chicago, matt.wu.key)
#' milw_conds <- PWS_conditions(milw_metadata, matt.wu.key)
#' basemap <- set_basemap(PWS.Conds.Chicago, zoom = 8)
#' gg_points(milw_conds, basemap, title = "Milwaukee PWS in Greater Chicago Context")
#'
#' ## this one-off hack works...
#' milw_metadata <- click_meta_query(PWS.Conds.Chicago, matt.wu.key)
#' basemap <- set_basemap(PWS.Conds.Chicago, zoom = 8)
#' ggmap::ggmap(basemap, extent = "device") +
#'  ggplot2::geom_point(data=milw_conds,
#'                      ggplot2::aes_string(x = 'longitude', y = 'latitude'),
#'                      col= "red",alpha =.8) +
#'  ggplot2::ggtitle("Milwaukee PWS in Greater Chicago Context")
#' }

# TODO figure out how to stick more of this in the function
basemap <- set_basemap(PWS.Conds.Chicago, zoom = 8)
gg_points(PWS.Conds.Chicago, basemap, title = "Centered on Downtown Chicago PWS")
click_meta_query <- function(PWS.Conditions, user_key){
  # returns cond object
  user_point <- ggmap::gglocator()
  PWS_meta_query(user_point[[1]], user_point[[2]], 50, user_key = user_key)
}
# milw_metadata <- click_meta_query(PWS.Conds.Chicago, matt.wu.key)
milw_metadata <- data(milw_metadata)
# milw_conds <- PWS_conditions(milw_metadata, matt.wu.key)
data(milw_conds)
ggmap::ggmap(basemap, extent = "device") +
  ggplot2::geom_point(data=milw_conds,
                      ggplot2::aes_string(x = 'longitude', y = 'latitude'),
                      col= "red",alpha =.8) +
  ggplot2::ggtitle("Milwaukee PWS in Greater Chicago Context")



## what function should do  ....
# basemap <- set_basemap(PWS.Conds.Chicago, zoom = 8)
# gg_points(PWS.Conds.Chicago, basemap, title = "Centered on Downtown Chicago PWS")
# user_point <- ggmap::gglocator()
# milw_metadata <- PWS_meta_query(user_point[[1]], user_point[[2]], 50, matt.wu.key <- "fd9858dfc94a85ea")

# devtools::use_data(milw_metadata)
# devtools::use_data(milw_conds)

#################### end prototype #############################################









### search radius
click_meta_query <- function(PWS.Class){
  # returns cond object
  basemap <- set_basemap(PWS.Class)
  gg_points(PWS.Class, basemap)
  user_point <- ggmap::gglocator()
  PWS_meta_query(user_point[[1]], user_point[[2]], 50,
                 matt.wu.key <- "fd9858dfc94a85ea")
}


basemap <- set_basemap(PWS.Conds.Chicago, zoom = 8)
gg_points(PWS.Conds.Chicago, basemap, title = "Centered on Downtown Chicago PWS")
user_point <- ggmap::gglocator()
# u_centre_table <- createCentroidTable(user_point[[1]], user_point[[2]], 100, 40)
milw_metadata <- PWS_meta_query(user_point[[1]], user_point[[2]], 50, matt.wu.key <- "fd9858dfc94a85ea")
milw_conds <- PWS_conditions(milw_metadata, matt.wu.key)
basemap <- set_basemap(PWS.Conds.Chicago, zoom = 8)
gg_points(milw_conds, basemap, title = "Milwaukee PWS in Greater Chicago Context")



spdf <- toSPntsDF(greater_chi_metadata[[1]])
sp::plot(spdf, main = "Greater Chicago PWS", col = "red")


webmap_pnts(PWS.Conds.Chicago)
webmap_pnts(PWS.Hist.Chicago) # doesn't work
webmap_pnts(PWS.Loc.Chicago) # doesn't work

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


## --- misc helpers ------------------------------------------------------------
# A local projection (Milwaukee, Wis.) for spatial calculations
NAD27 <- CRS("+proj=lcc +lat_1=42.73333333333333 +lat_2=44.06666666666667
             +lat_0=42 +lon_0=-90 +x_0=609601.2192024384 +y_0=0 +datum=NAD27
             +units=us-ft +no_defs +ellps=clrk66 +nadgrids=@conus,
             @alaska,@ntv2_0.gsb,@ntv1_can.dat")

# Web Mercator projection for web mapping
WGS84 <- sp::CRS("+proj=longlat +datum=WGS84")

# transform CRS to Web Merator for web mapping
toWGS84 <- function(sp) {
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84")
  sp::spTransform(sp, WGS84)
}



## towards fixing draw_subset function; subsets only top slot, not conditions @data:
data(PWS.Conds.Chicago)
sp::plot(PWS.Conds.Chicago, col="red", pch = 20, add = FALSE)
simple_pnts(PWS.Conds.Chicago, col="red", pch = 20, add = FALSE)
my_ss  <-  draw_subset(PWS.Conds.Chicago)
sp::plot(my_ss@spatialPtDF)


### test scripts ###

# test_that("get_cdb_table",{
#   #
#   # Check that the function produces correct errors if arguments are worngly specified
#   expect_error(get_cdb_table("public.stations", 12345),
#                "cdb_account must be of type character.")
#
#   expect_error(get_cdb_table(112233, "your.cdb.account"),
#                "table name (of CartoDB table) must be of type character.")
# })

# get_cdb_table("public.stations", your.cdb.account)
