##
## Begin jamarin code
##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# The PWS.Conditions S4 class required for the projects is located herein along with                  +
# functions related to its creation and initialization. Relevant validation testing is              +
# done in the initialization function of the class.
# +
# + Data sets which are included and which show the output of those functions:                      +
# +                                                                                                 +
# + o PWS.Conds.Chicago.rda                                                                           +
# +                                                                                                 +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# global.Conds.env <- new.env(parent = emptyenv())
# global.Conds.env$repository = list()
# length(global.Conds.env$repository)

#' An S4 class to represent the PWS Conditions of a user search
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sp SpatialPoints
#' @export
#' @slot spatialPtDF A SpatialPointsDataFrame
#' @slot spatialPt A SpatialPoints Matrix
#' @slot call list of user's desired location coordinates and radius
#' @slot data data
#'
setClass(
  Class = "PWS.Conditions",
  slots = c(spatialPtDF="ANY", spatialPt="ANY", call="list", data="data.frame")
)

#' PWS.Conditions constructor function
#' @return S4 PWS.Condition class
#' @param ... coordinates of PWS.Conditions initializer function
PWS.Conditions <- function(...) return(new(Class="PWS.Conditions",...))

#' S4 PWS.Conditions Initializer function
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sp SpatialPoints
#' @export
#' @param .Object S4 initializer object
#' @param PWS.Locations A PWS.Locations S4 Class Object
#' @param user.key character vector of user.key
#' @param ... other user input
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

#' Chicago Conditions Dataset
#'
#' This contains the weather conditions for the Personal Weather Stations in
#' a 5km region centered in Chicago, Illinois from March 13, 2016
#' called with PWS.Conds.Chicago <- PWS.Conditions(PWS.Loc.Chicago, user.key=jam.key.2)
#' where the PWS.Loc.Chicago call is described in the PWS.Locations S4 Class
#'
#' @examples
#' data(PWS.Conds.Chicago)
#' @author wundr team
#'
"PWS.Conds.Chicago"
##
## End jamarin code
##
##



