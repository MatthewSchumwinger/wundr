#require(sp); require(geosphere)

#' An S4 class to represent a bank account.
#' @include S4.Locations.R
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sp SpatialPoints
#' @export
#' @slot spatialPtDF A length-one numeric vector
#' @slot spatialPt A legnth-one numeric vector
#' @slot call list of stuff
#'
setClass(
  Class = "PWS.Query.Subset",
  slots = c(spatialPtDF="SpatialPointsDataFrame", spatialPt="SpatialPoints", call="list")
)

PWS.Query.Subset <- function(...) return(new(Class="PWS.Query.Subset",...))

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

  desired.s.point <- SpatialPoints( cbind(longitude, latitude) )
  desired.s.point@proj4string = CRS( "+proj=longlat +ellps=WGS84" )

  p <- Polygon( PWS.Locations@spatialPt@coords[chull(PWS.Locations@spatialPt@coords),] )
  ps1 <- Polygons(list(p), ID="a")
  Locations.Spatial.Polygon <- SpatialPolygons( list(ps1), proj4string = CRS("+proj=longlat +ellps=WGS84") )

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
    circle <- t(sapply(1:360, function(degrees) destPoint(desired.s.point,degrees,radius*1000)))

    if( nrow(queries)!=0 & any(is.na( SpatialPoints(circle, CRS("+proj=longlat +ellps=WGS84") ) %over%
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

##
## EXAMPLE SUBSETTING
## PWS.Sub <- PWS.Query.Subset(PWS.L, -118.49, 34.02, 2)
# PWS.Locations.Query@spatialPtDF@bbox
# a@call
# isS4(PWS.MetaQuery)
# a <- PWS.L


