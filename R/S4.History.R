##
## Begin jamarin (John A. Marin) code
##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# + The PWS.History S4 class organizes history information for user searches                        +
# + and spontaneously provides a variety of information about the PWS stations including            +
# + average, variance, and standard deviation data about Temperature, Humidity, Pressure,           +
# + and Dewpoint                                                                                    +
# + Data sets which are included and which show the output of those functions:                      +
# +                                                                                                 +
# + o PWS.Hist.Chicago.rda                                                                          +
# +                                                                                                 +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' PWS.History
#' An S4 class to store history data related to PWS searches
#' @importFrom lubridate now
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sp SpatialPoints
#' @return S4 class
#' @slot average.Temp values
#' @slot average.Humidity values
#' @slot average.Pressure values
#' @slot average.Dew.Point values
#'
#' @slot variance.Temp values
#' @slot variance.Humidity values
#' @slot variance.Pressure values
#' @slot variance.Dew.Point values
#'
#' @slot standard.Dev.Temp values
#' @slot standard.Dev.Humidity values
#' @slot standard.Dev.Pressure values
#' @slot standard.Dev.Dew.Point values
#'
#' @slot spatialPtDF ANY SpatialPointsDataFrame
#' @slot spatialPt ANY SpatialPoints
#' @slot call list of query search
#' @slot history data
#' @export
#'
setClass(
  Class = "PWS.History",
  slots = c(average.Temp="data.frame",
            average.Humidity="data.frame",
            average.Pressure="data.frame",
            average.Dew.Point="data.frame",

            variance.Temp="data.frame",
            variance.Humidity="data.frame",
            variance.Pressure="data.frame",
            variance.Dew.Point="data.frame",

            standard.Dev.Temp="data.frame",
            standard.Dev.Humidity="data.frame",
            standard.Dev.Pressure="data.frame",
            standard.Dev.Dew.Point="data.frame",

            history="data.frame",
            spatialPtDF="ANY",
            spatialPt="ANY",
            call = "list"
  )
)
#' PWS.History constructor function
#' @return S4 class
#' @export
#' @param ... coordinates of initializer function
PWS.History <- function(...) return(new(Class="PWS.History",...))

#' S4 Initializer function
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sp SpatialPoints
#' @importFrom lubridate now
#' @export
#' @param .Object S4 initializer object
#' @param PWS.Locations S4 Object Required
#' @param begin_YYYYMMDD begin of query
#' @param end_YYYYMMDD end of query
#' @param user.key user.key of user
#' @param imperial whether the user wants imperial or metric
#' @param ... Passed on to next function
#'
setMethod("initialize",
          "PWS.History",

          function(.Object, PWS.Locations, begin_YYYYMMDD, end_YYYYMMDD, user.key, imperial=TRUE,...){

            if (!isS4(PWS.Locations)) stop("Please use the PWS.Locations() to create an S4 wundr object.")

            if (typeof(begin_YYYYMMDD)!="character" | typeof(end_YYYYMMDD)!="character" ) {stop("Please note that the Date must be of type character.")}

            if ( as.Date(begin_YYYYMMDD, "%Y%m%d") < as.Date("19450101", "%Y%m%d")  |
                 as.Date(end_YYYYMMDD, "%Y%m%d") < as.Date("19450101", "%Y%m%d") )  {
              stop("Wunderground Historical Archives only go back to Jan 1, 1945.")
            }

            if ( as.Date(begin_YYYYMMDD, "%Y%m%d") >  as.Date(substring(now(),0,10), "%Y-%m-%d")  |
                 as.Date(end_YYYYMMDD, "%Y%m%d") > as.Date(substring(now(),0,10), "%Y-%m-%d") ) {
              stop("Historical dates must not be in the future.")
            }

            if ( as.Date(begin_YYYYMMDD, "%Y%m%d") > as.Date(end_YYYYMMDD, "%Y%m%d") ) {
              stop("Your Begin Date must precede your End Date.")
            }

            if (typeof(user.key)!="character") stop("Please note that the user.key must be of type character.")

            history <- PWS_history(list(PWSmetadata = PWS.Locations@spatialPtDF@data), begin_YYYYMMDD, end_YYYYMMDD, user.key)

            if (imperial) {

              .Object@average.Temp <- aggregate(tempi ~ id, data=history, mean)
              .Object@average.Humidity <- aggregate(hum ~ id, data=history, mean)
              .Object@average.Pressure <- aggregate(pressurei ~ id, data=history, mean)
              .Object@average.Dew.Point <- aggregate(dewpti ~ id, data=history, mean)

              .Object@variance.Temp <- aggregate(tempi ~ id, data=history, var)
              .Object@variance.Humidity <- aggregate(hum ~ id, data=history, var)
              .Object@variance.Pressure <- aggregate(pressurei ~ id, data=history, var)
              .Object@variance.Dew.Point <- aggregate(dewpti ~ id, data=history, var)

              .Object@standard.Dev.Temp <- aggregate(tempi ~ id, data=history, sd)
              .Object@standard.Dev.Humidity <- aggregate(hum ~ id, data=history, sd)
              .Object@standard.Dev.Pressure <- aggregate(pressurei ~ id, data=history, sd)
              .Object@standard.Dev.Dew.Point <- aggregate(dewpti ~ id, data=history, sd)

            }else{

              .Object@average.Temp <- aggregate(tempm ~ id, data=history, mean)
              .Object@average.Humidity <- aggregate(hum ~ id, data=history, mean)
              .Object@average.Pressure <- aggregate(pressurem ~ id, data=history, mean)
              .Object@average.Dew.Point <- aggregate(dewptm ~ id, data=history, mean)

              .Object@variance.Temp <- aggregate(tempm ~ id, data=history, var)
              .Object@variance.Humidity <- aggregate(hum ~ id, data=history, var)
              .Object@average.Pressure <- aggregate(pressurem ~ id, data=history, var)
              .Object@average.Dew.Point <- aggregate(dewptm ~ id, data=history, var)

              .Object@standard.Dev.Temp <- aggregate(tempm ~ id, data=history, sd)
              .Object@standard.Dev.Humidity <- aggregate(hum ~ id, data=history, sd)
              .Object@average.Pressure <- aggregate(pressurem ~ id, data=history, sd)
              .Object@average.Dew.Point <- aggregate(dewptm ~ id, data=history, sd)

            }

            .Object@call <- PWS.Locations@call

            .Object@spatialPtDF <- PWS.Locations@spatialPtDF

            .Object@spatialPt <- PWS.Locations@spatialPt

            .Object@history <- history

            return(.Object)
          }
)

#' Chicago History Dataset
#'
#' This contains the weather history for the Personal Weather Stations in
#' a 5km region centered in Chicago, Illinois from March 13, 2016
#' called with PWS.Hist.Chicago <- PWS.History(PWS.Loc.Chicago, "20150306", "20150310", user.key)
#' where the PWS.Loc.Chicago call is described in the PWS.Locations S4 Class
#'
#' @examples
#' data(PWS.Hist.Chicago)
#' @author wundr team
#'
"PWS.Hist.Chicago"

#' Chicago 10-day Forecast
#'
#' This contains at 10-day Forecast over all the PWS using wundr's kriging method

#' @examples
#' data(Chicago.forecast.10day)
#' @author wundr team
#'
"Chicago.forecast.10day"

##
## End jamarin code
##
##


