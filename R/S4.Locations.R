
## library(sp); library(jsonlite); library(httr)

#' j.geocode
#'
#'
#' @importFrom jsonlite fromJSON
#'
#' @param address name
#' @return Coordinates of desired location
#' @export
#' @examples
#' j.geocode("Santa Monica, CA")
#'
j.geocode <- function(address){
  u <- sprintf("http://maps.googleapis.com/maps/api/geocode/json?address=%s",
               gsub('\\s+', '+', enc2utf8(address)))
  query <- jsonlite::fromJSON(content(GET(u), as='text'))
  data <- query$results$geometry$location
  return(as.numeric(data))
}

setClass(
  Class = "PWS.Locations",
  slots = c(spatialPtDF="SpatialPointsDataFrame", spatialPt="SpatialPoints", call="list")
)

PWS.Locations <- function(...) return(new(Class="PWS.Locations",...))


setMethod("initialize",
          "PWS.Locations",
          function(.Object, longitude, latitude, radius, user.key){


            if (radius<=0) stop("Please note that the search radius must be positive.")
            if (typeof(user.key)!="character") stop("Please note that the user.key must be of type character.")

            args.length <- length(as.list(match.call()[-1]))
            if ( is.numeric(longitude) & (args.length == 5) ) {
              Meta.DF <- PWS_meta_query(longitude, latitude, radius, user_key=user.key)
              if (longitude < -180 | longitude > 180) stop("Please note that longitude must be -/+180.")
              if (latitude < -90 | latitude > 90) stop("Please note that latitude must be -/+90.")
            }else{
              location <- longitude
              g <- j.geocode(location)
              Meta.DF <-PWS_meta_query(g[2], g[1], radius, user_key=user.key)
            }

            coords <- cbind(Meta.DF$PWSmetadata$lon, Meta.DF$PWSmetadata$lat)
            .Object@spatialPt <- SpatialPoints(coords)
            .Object@spatialPtDF <- SpatialPointsDataFrame(.Object@spatialPt, Meta.DF$PWSmetadata)
            .Object@call <- Meta.DF$call
            return(.Object)
          }
)


#PWS.Locations("Santa Monica, CA", radius=3, user.key=user.key)

#PWS.Locations(-118.49119, 34.01945, radius=3, user.key=user.key)

