##
## Begin jamarin (John A. Marin) code
##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +                                                                                                 +
# + The S4.Methods include a plot() function that is specific to PWS.Conditions,                    +
# + PWS.Locations,  and PWS.Query.Subset S4 objects as well as 'getter' methods accessing           +
# + the 'conditions' environmental variable (i.e. `PWS.temp()`, `PWS.humidity()`, etc.)             +
# +                                                                                                 +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#' @include S4.Locations.R
#' @import methods
#'
setMethod("plot", signature(x=c("PWS.Locations")),
          function(x,y,...) {
            webmap_pnts(x)
          })

#' @include S4.Conditions.R
setMethod("plot", signature(x=c("PWS.Conditions")),
          function(x,y,...) {
            webmap_pnts(x)
          })

#' @include S4.Query.Subset.R
setMethod("plot", signature(x=c("PWS.Query.Subset")),
          function(x,y,...) {
            webmap_pnts(x)
          })

#' PWS.temp
#' @keywords IO
#' @export
#'
PWS.temp <- function() {
  if ( length(wundr.env$conds) == 0 ) { stop("Please create a PWS.Conditions() Object before proceeding.")
  }else{
    temp_f = wundr.env$conds$temp_f
    PWS_station = wundr.env$conds$station_id
    City = wundr.env$conds$city
    data.frame( temp_f, PWS_station, City )
  }}

#' PWS.humidity
#' @keywords IO
#' @export
#'
PWS.humidity <- function() {
  if ( length(wundr.env$conds) == 0 ) { stop("Please create a PWS.Conditions() Object before proceeding.")
  }else{
    relative.humidity = wundr.env$conds$relative_humidity
    PWS_station = wundr.env$conds$station_id
    City = wundr.env$conds$city
    data.frame( relative.humidity, PWS_station, City )
  }}

#' PWS.feels.like
#' @keywords IO
#' @export
#'
PWS.feels.like <- function() {
  if ( length(wundr.env$conds) == 0 ) { stop("Please create a PWS.Conditions() Object before proceeding.")
  }else{
    feels.like = wundr.env$conds$feelslike_string
    PWS_station = wundr.env$conds$station_id
    City = wundr.env$conds$city
    data.frame( feels.like, PWS_station, City )
  }}

#' PWS.dewpoint
#' @keywords IO
#' @export
#'
PWS.dewpoint <- function() {
  if ( length(wundr.env$conds) == 0 ) { stop("Please create a PWS.Conditions() Object before proceeding.")
  }else{
    dewpoint = wundr.env$conds$dewpoint_string
    PWS_station = wundr.env$conds$station_id
    City = wundr.env$conds$city
    data.frame( dewpoint, PWS_station, City )
  }}

##
## End jamarin code
##





