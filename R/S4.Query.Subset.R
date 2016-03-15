##
## Begin jamarin (John A. Marin) code
##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# The PWS.Query.Subset S4 class provides regional subset information for user searches              +
# +                                                                                                 +
# + o PWS.Loc.Sub.Chicago.rda                                                                       +
# +                                                                                                 +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' An S4 class to represent a bank account.
#' @include S4.Locations.R
#' @importFrom sp Polygon
#' @importFrom sp Polygons
#' @importFrom sp CRS
#' @importFrom sp SpatialPolygons
#' @importFrom sp %over%
#' @importFrom geosphere destPoint
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sp SpatialPoints
#' @export
#' @slot spatialPtDF A SpatialPointsDataFrame
#' @slot spatialPt A SpatialPoints
#' @slot call list of stuff
#'
#'
setClass(
  Class = "PWS.Query.Subset",
  slots = c(spatialPtDF="ANY", spatialPt="ANY", call="list")
)

#' PWS.Query.Subset constructor function
#' @return S4 PWS.Query.Subset class
#' @param ... Additional Details Specified by User
PWS.Query.Subset <- function(...) return(new(Class="PWS.Query.Subset",...))

#' S4 PWS.Query.Subset Initializer function
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sp SpatialPoints
#' @export
#' @param .Object S4 initializer object
#' @param PWS.Locations A PWS.Locations S4 Class Object
#' @param longitude User's Longitude of Choice
#' @param latitude User's Latitude of Choice
#' @param radius desired within confines of larger search
#' @param ... other user input
#'
setMethod("initialize",
          "PWS.Query.Subset",

          function(.Object, PWS.Locations, longitude, latitude, radius,...){

            if (!isS4(PWS.Locations)) stop("Please use the PWS.Locations() to create an S4 wundr object.")
            if (longitude < -180 | longitude > 180) stop("Please note that longitude must be -/+180.")
            if (latitude < -90 | latitude > 90) stop("Please note that latitude must be -/+90.")
            if (radius<=0) stop("Please note that the search radius must be positive.")

            tmp <- PWS.Query.Subset.fxn(PWS.Locations, longitude, latitude, radius, km_miles=TRUE)

            coords <- cbind(tmp[[1]]$lon, tmp[[1]]$lat)

            .Object@spatialPt <- SpatialPoints(coords)

            locations.data <- merge(as.data.frame(tmp[1]), PWS.Locations@spatialPtDF@data)

            .Object@spatialPtDF <- SpatialPointsDataFrame(.Object@spatialPt, locations.data)

            .Object@call <- tmp[[2]]

            return(.Object)
          }
)

PWS.Query.Subset.fxn <- function(PWS.Locations, longitude, latitude, radius, km_miles=TRUE){

  mile_per_km = 0.621371192237; m_per_mile = 1609.344; m_per_km = 1000
  options( warn=-1 )
  # The function internally uses km, if radius give in miles, convert:
  if(!km_miles) radius = radius/mile_per_km

  desired.s.point <- sp::SpatialPoints( cbind(longitude, latitude) )
  desired.s.point@proj4string = sp::CRS( "+proj=longlat +ellps=WGS84" )

  p <- sp::Polygon( PWS.Locations@spatialPt@coords[chull(PWS.Locations@spatialPt@coords),] )
  ps1 <- sp::Polygons(list(p), ID="a")
  Locations.Spatial.Polygon <- sp::SpatialPolygons( list(ps1), proj4string = sp::CRS("+proj=longlat +ellps=WGS84") )

  if(is.na(desired.s.point %over% Locations.Spatial.Polygon)){

    stop("Your point of interest does not lie within your initial PWS.Location search.")

  }else{
    meta.queries <- PWS.Locations@spatialPtDF@data
    queries <- meta.queries[meta.queries$distance_km < radius,]
    queries$distance_mi <- queries$distance_km * mile_per_km
    queries <- queries[order(queries$distance_km),]
    ##
    ## HAS USER'S DESIRED RADIUS EXCEEDED THE BOUNDARY REGION OF THE INITIAL LOCATIONS QUERY?
    ##
    circle <- t(sapply(1:360, function(degrees) geosphere::destPoint(desired.s.point,degrees,radius*1000)))

    if( nrow(queries)!=0 & any(is.na( SpatialPoints(circle, sp::CRS("+proj=longlat +ellps=WGS84") ) %over%
                                      Locations.Spatial.Polygon ))){
      cat("Your Subset Query has been returned, but please be advised your search radius\n")
      cat("exceeded the boundary region of your initial search.\n \n")

    }else{}
    ##
    ## LIST FORMATTED RETURN TO BE RENDERED TO S4 SLOTS
    ##
    if(nrow(queries)==0) queries <- NULL
    call=list("lon" = longitude,"lat" = latitude, "radius_km" = radius)
    list("PWS.Query.Subset" <- queries, "call" <- call)
  }
}

#' subRegion.Pnts
#'
#' #' Plots PWS locations and subregions on a contextual basemap.
#'
#' @importFrom ggmap ggmap
#' @importFrom ggplot2 geom_point aes_string
#' @param PWS.class A PWS.class points S4 object.
#' @param PWS.sub A subregion of the initial PWS.class
#' @param title A character string title for the map. Default = NULL.
#' @param ... ...
#' @return NULL
#' @export
#' @examples
#' data("PWS.Loc.Chicago")
#' data("PWS.Loc.Sub.Chicago")
#' subRegion.Pnts(PWS.Loc.Chicago, PWS.Loc.Sub.Chicago)
#'
subRegion.Pnts <- function(PWS.class, PWS.sub, title = NULL, ...) {

  pnts <- PWS.class@spatialPtDF@data
  pnts.sub <- PWS.sub@spatialPtDF@data
  basemap <- set_basemap(PWS.class, zoom=13)

  ggmap::ggmap(basemap, extent = "device") +

    ggplot2::geom_point(data=pnts,
                        ggplot2::aes_string(x = 'lon', y = 'lat'),
                        col= "red",alpha = 1) +
    ggplot2::geom_point(data=pnts.sub,
                        ggplot2::aes_string(x = 'lon', y = 'lat'),
                        col= "black",alpha = 1) +
    ggplot2::ggtitle(title)
}

#' Chicago History Dataset
#'
#' This contains subregion locations for the Personal Weather Stations in
#' a 2km region centered near Chicago, Illinois from March 13, 2016
#' called with PWS.Loc.Sub.Chicago <- PWS.Query.Subset(PWS.Loc.Chicago, -87.62, 41.88, 2)
#' where the PWS.Loc.Chicago call is described in the PWS.Locations S4 Class (which is a 5km region).
#' The subregion lies within the larger region.
#'
#' @examples
#' data(PWS.Loc.Sub.Chicago.rda)
#' @author wundr team
#'
"PWS.Loc.Sub.Chicago"

##
## End jamarin code
##





