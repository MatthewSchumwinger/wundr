##
## Begin jamarin code
##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# The PWS.Location S4 class required for the projects is located herein along with                  +
# functions related to its creation and initialization. Relevant validation testing is              +
# done in the initialization function of the class.
# +
# + Data sets which are included and which show the output of those functions:                      +
# +                                                                                                 +
# + o PWS.Loc.Chicago.rda                                                                           +
# +                                                                                                 +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' j.geocode
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom httr content
#' @param address user character input
#' @export
#' @return A vector of a (longitude,latitude) pair in numeric format
#' @examples
#' \dontrun{j.geocode("Santa Monica, CA")}
#'
j.geocode <- function(address){
  u <- sprintf("http://maps.googleapis.com/maps/api/geocode/json?address=%s",
               gsub('\\s+', '+', enc2utf8(address)))
  query <- jsonlite::fromJSON(httr::content(httr::GET(u), as='text'))
  data <- query$results$geometry$location
  return(as.numeric(data))
}

#' PWS.Locations
#' An S4 class to store and display data related to PWS searches
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sp SpatialPoints
#' @export
#' @return S4 class
#' @slot spatialPtDF ANY SpatialPointsDataFrame
#' @slot spatialPt ANY SpatialPoints
#' @slot call list of called coordinates
#'
setClass(
  Class = "PWS.Locations",
  slots = c(spatialPtDF="ANY", spatialPt="ANY", call="list")
)

#' PWS.Locations constructor function
#' @return S4 class
#' @param ... coordinates of initializer function
PWS.Locations <- function(...) return(new(Class="PWS.Locations",...))

#' S4 Initializer function
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sp SpatialPoints
#' @export
#' @param .Object S4 initializer object
#' @param longitude desired longitude
#' @param latitude desired latitude
#' @param radius desired radius
#' @param user.key user.key of user
#'
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

#' Chicago Locations Dataset
#'
#' This contains a listing of the Personal Weather Stations in
#' a 5km region centered in Chicago, Illinois from March 13, 2016
#' (PWS.Loc.Chicago <- PWS.Locations("Chicago, IL", radius=5, user.key)).
#'
#' @examples
#' data(PWS.Loc.Chicago)
#' head(PWS.Loc.Chicago)
#' @author wundr team
#'
"PWS.Loc.Chicago"
#'
#' ##
## End jamarin code
##

