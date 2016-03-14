##
## Begin jamarin code
##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# + The S4 classes required for the projects are located herein.  Functions related to the
# creation and initalization functions are also contained below.  Furthermore, relevant
# validation functions are attached as well since their presence in the initalization function
# generally make other 'setValidation' functions largely redundant.
#
# The author endeavored to take the json-based API output from the low level functions and render
# it into a useful format for developers and researchers alike.  The slots include 'SpatialPointsDataFrame'
# and 'SpatialPoints' thusly an extension of R's most popular geospatial package 'sp'.
#
# + Those API functions form Part 1 of the projects and are used in later parts of the project      +
# + by either calling them directly or incorporating parts of them. While the user is supposed to   +
# + interact with those functions through the S4 class, we have sufficiently documented the func-   +
# + tions, making it possible to use them indepentently.                                            +
# +                                                                                                 +
# + The functions include:                                                                          +
# +                                                                                                 +
# + o createCentroidTable                                                                           +
# + o PWS_meta_query                                                                                +
# + o PWS_meta_subset                                                                               +
# + o PWS_conditions                                                                                +
# + o PWS_history                                                                                   +
# +                                                                                                 +
# + There are also data sets which are included and which show the output of those functions:       +
# +                                                                                                 +
# + o Rio_basemap                                                                                  +
# + o Rio_metadata                                                                                  +
# + o Rio_conditions                                                                                +
# + o Rio_history                                                                                   +
# +                                                                                                 +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




#require(sp)
## library(sp); library(jsonlite); library(httr)

#' j.geocode
#'
#'
#' @importFrom jsonlite fromJSON
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

#' An S4 class to represent a bank account.
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sp SpatialPoints
#' @export
#' @slot spatialPtDF A length-one numeric vector
#' @slot spatialPt A legnth-one numeric vector
#' @slot call list of stuff
#'
setClass(
  Class = "PWS.Locations",
  slots = c(spatialPtDF="SpatialPointsDataFrame", spatialPt="SpatialPoints", call="list")
)

PWS.Locations <- function(...) return(new(Class="PWS.Locations",...))

#' An S4 class to represent a bank account.
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sp SpatialPoints
#' @slot spatialPtDF A length-one numeric vector
#' @export
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


# PWS.Locations("Santa Monica, CA", radius=3, user.key=jam.key)
#
# PWS.L <- PWS.Locations(-118.49119, 34.01945, radius=3, user.key=jam.key)
# PWS.L
# PWS.L@spatialPtDF

#PWS.Locations("Santa Monica, CA", radius=3, user.key=user.key)

#PWS.Locations(-118.49119, 34.01945, radius=3, user.key=user.key)


