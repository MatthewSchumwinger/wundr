#require(lubridate)

#' An S4 class to represent a bank account.
#' @include S4.Locations.R
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sp SpatialPoints
#' @export
#' @slot spatialPtDF A length-one numeric vector
#' @slot spatialPt A legnth-one numeric vector
#' @slot call list of stuff
#' @slot data data
#'
setClass(
  Class = "PWS.Conditions",
  slots = c(spatialPtDF="SpatialPointsDataFrame", spatialPt="SpatialPoints", call="list", data="data.frame")
)

PWS.Conditions <- function(...) return(new(Class="PWS.Conditions",...))

#PWSmetadata <- PWS.Locations@spatialPtDF@data # feeds into lower level function as

setMethod("initialize",
          "PWS.Conditions",

          function(.Object, PWS.Locations, user.key,...){

            if (!isS4(PWS.Locations)) stop("Please use the PWS.Locations() to create an S4 wundr object.")

            if (typeof(user.key)!="character") stop("Please note that the user.key must be of type character.")

            .Object@call <- PWS.Locations@call

            .Object@spatialPtDF <- PWS.Locations@spatialPtDF

            .Object@spatialPt <- PWS.Locations@spatialPt

            .Object@data <- PWS_conditions(list(PWSmetadata = PWS.Locations@spatialPtDF@data), user.key)

            return(.Object)
          }
)
##
##  EXAMPLE
##

# C.S4 <- PWS.Conditions(PWS.L, jam.key)
# C.S4.sub <- PWS.Conditions(PWS.Sub, jam.key)
# View(C.S4.sub@data)
# h.S4.sub
#
# plot(C.S4)
#
# View(C.S4@data)


