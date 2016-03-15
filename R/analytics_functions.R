# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# + This document contains analytics functions for both temporal as well as spatial data analysis   +
# + on data obtained from Weather Underground through the API functions PWS_history or              +
# + PWS_conditions, as well as the corresponding S4 functions which are based on them. Those API    +
# + functions form Part 5 of the project and are used in other parts of the project by either       +
# + calling them directly or incorporating parts of them.                                           +
# +                                                                                                 +
# + The functions for temporal analysis include:                                                    +
# +                                                                                                 +
# + o history_zoo                                                                                   +
# + o history_ts                                                                                    +
# + o history_forecast                                                                              +
# +                                                                                                 +
# + The functions for spatial analysis include:                                                     +
# +                                                                                                 +
# + o create_geo_cond                                                                               +
# + o create_grid                                                                                   +
# + o GP_fit                                                                                        +
# +                                                                                                 +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##
## Begin zohren code
##


#require(zoo);require(forecast);

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +                                    TEMPORAL ANALYSIS                                            +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




#' history_zoo
#'
#' Irregular times series object of historical weather data. Takes as input a data frame of historical
#' weather data e.g. as created by 'PWS_history' or corresponding S4 function. It selects the data for a
#' given station and given variable(s) and returns a time series object of class 'zoo' (which is designed
#' for irregular time series).
#'
#' @importFrom zoo zoo
#'
#' @param hist.data data frame of historical weather data (e.g. as created by 'PWS_history')
#' @param id Personal Weather Station ID
#' @param variables Variable(s) to be selected as character (can be a vector, see example below)
#' @return Time series object of type 'zoo' with selected variables measured by selected weather station
#' @export
#' @examples
#' data(Rio_history)
#' hist.zoo <- history_zoo(Rio_history,"IRIODEJA53",c("hum","tempm"))
#' plot(hist.zoo,col='red', main = "Humidity and Temperatur")
#'
history_zoo <- function(hist.data,id,variables){
  hist.data <- hist.data[hist.data$id==id,c("year","mon","mday","hour","min",variables)]
  hist.data$time <- as.POSIXct(strptime(paste(hist.data$year,hist.data$mon,hist.data$mday,hist.data$hour,hist.data$min,sep = "-"),
                    "%Y-%m-%d-%H-%M"))
  hist.data.zoo <- zoo::zoo(hist.data[,variables], order.by=hist.data$time)
  if(length(hist.data.zoo)==0) stop("Time series is empty.")
  hist.data.zoo
}







#' history_ts
#'
#' Regular times series object of historical weather data. Takes as input a data frame of historical
#' weather data e.g. as created by 'PWS_history' or corresponding S4 function. It selects the data for a
#' given station and given variable(s) and returns a time series object of class 'ts'.
#'
#' @importFrom zoo zoo zooreg
#' @importFrom stats as.ts
#'
#' @param hist.data data frame of historical weather data (e.g. as created by 'PWS_history')
#' @param id Personal Weather Station ID
#' @param variables Variable(s) to be selected as character (can be a vector, see example below)
#' @return Time series object of type 'ts' with selected variables measured by selected weather station
#' @export
#' @examples
#' data(Rio_history)
#' hist.ts <- history_ts(Rio_history,"IRIODEJA53","hum")
#' plot(hist.ts,col='red', main = "Humidity")
#'
history_ts <- function(hist.data,id,variables){
  hist.data.zoo <- history_zoo(hist.data,id,variables)
  stats::as.ts(zoo::zooreg(hist.data.zoo))
}






#' history_forecast
#'
#' Forecasts of time series created from 'history_zoo' or 'history_ts'. Note that the time series must be univariate.
#'
#' @importFrom zoo zoo zooreg
#' @importFrom forecast forecast
#' @importFrom stats as.ts
#'
#' @param history.tszoo Time series object from history_ts or history_zoo
#' @param find.frequency Boolean. Whether to find frequency automatically (default is TURE).
#' @param ... Additional parameters to be passed to 'forecast'
#' @return Time series object of type 'ts' with selected variables measured by selected weather station
#' @export
#' @examples
#' data(Rio_history)
#' hist.ts <- history_ts(Rio_history,"IRIODEJA53","hum")
#' hist.forecast <- history_forecast(hist.ts)
#' plot(hist.forecast, main = 'Forecast', xlab='Time (days)', ylab='Humidity (%)')
#'
history_forecast <- function(history.tszoo, find.frequency=TRUE,... ){
  if(!("zoo" %in% class(history.tszoo)) & !("ts" %in% class(history.tszoo) ))
    stop("Argument must be a time series of class 'zoo' or 'ts'.")
  if("zoo" %in% class(history.tszoo) )
    history.tszoo <- stats::as.ts(zoo::zooreg(history.tszoo))
  if(!is.null(dim(history.tszoo))) stop("Forecasting is only possible for univariate time series.")
  forecast::forecast(history.tszoo,find.frequency=find.frequency,...)
}






# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +                                     SPATIAL ANALYSIS                                            +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#' create_geo_cond
#'
#' Takes as input a data frame of conditions from weather stations and selects coordinates as well as
#' conditions for one variable and saves them as a geodata object
#'
#' @importFrom geoR as.geodata
#'
#' @param data.conditions Data frame with data on conditions for various weather stations
#' @param variable Name of variable to select from data frame
#'
#' @return A object of type geodata containing coordinates (lon,lat) and values for selected variable
#' @export
#' @examples
#' data(Rio_conditions)
#' data.geo <- create_geo_cond(Rio_conditions,"temp_c")
#' data.geo
#'
create_geo_cond <- function(data.conditions,variable){
  if(length(variable)>1) stop("Select only one variable.")
  if(!(variable %in% colnames(data.conditions))) stop("undefined columns selected")
  coord.col <- c(which( colnames(data.conditions)=="longitude" ),which( colnames(data.conditions)=="latitude" ))
  geoR::as.geodata(data.conditions, coords.col=coord.col, data.col =  which( colnames(data.conditions)==variable ))
}


#' create_grid
#'
#' Creates a grid of points in a rectangular over a sets of points coordinates from a geodata object
#' or any other object with coordinates saves in an attribute called 'coords'
#'
#'
#' @param data.geo geodata object with coordinates in attribute called 'coords'
#' @param size.lon Discrte size (number of grid points) in longitudinal direction (defaultis 50).
#' @param size.lat Discrte size (number of grid points) in latitudinal direction (default is 50).
#' @param grid.lim Limiting values of longitude and latitude for grid. When set to null, it chooses the max/min value of longitude and latitude in coord, otherwise a vector c(min.lon,max.lon,min.lat,max.lat) must be provided
#' @return A data frame containing the grid points with columns labelled (lon,lat)
#' @export
#' @examples
#' data(Rio_conditions)
#' data.geo <- create_geo_cond(Rio_conditions,"temp_c")
#' grid.positions <- create_grid(data.geo)
#' grid.positions <- create_grid(data.geo, grid.lim= c(-43.6,-42.8,-23.2,-22.6) )
#' head(grid.positions)
#'
create_grid <- function(data.geo, size.lon=50,size.lat=50, grid.lim=NULL){
  if(class(data.geo)!="geodata") stop("First argument must be of class 'geodata'.")
  if(length(grid.lim)!=4 | typeof(grid.lim)!="double"){
    min.lon <- min(data.geo$coords[,1])
    max.lon <- max(data.geo$coords[,1])
    min.lat <- min(data.geo$coords[,2])
    max.lat <- max(data.geo$coords[,2])
  } else {
    min.lon = grid.lim[1]
    max.lon = grid.lim[2]
    min.lat = grid.lim[3]
    max.lat = grid.lim[4]
  }
  lon.range <- seq(min.lon, max.lon, by=(max.lon-min.lon)/size.lon)
  lat.range <- seq(min.lat, max.lat, by=(max.lat-min.lat)/size.lat)
  grid.positions <- as.matrix(cbind(rep(lon.range,length(lat.range)),
                                    rep(lat.range, each=length(lon.range))))
  colnames(grid.positions) <- c("lon","lat")
  grid.positions
}




#' GP_fit
#'
#' A Gaussian process fit (Kriging) on geodata object.
#'
#' @importFrom geoR ksline
#' @importFrom ggplot2 ggplot geom_raster geom_point scale_colour_gradient
#'
#' @param data.geo geodata object with coordinates and variable
#' @param ... Additional parameters to be passed to create_grid, i.e. for changing the grid limitations
#' @return A Gaussian process fit of the variable on all points on a rectangular grid spanning the region
#' @export
#' @examples
#' data(Rio_conditions)
#' data.geo <- create_geo_cond(Rio_conditions,"temp_c")
#' model<-GP_fit(data.geo)
#' ggplot2::ggplot(data = model, ggplot2::aes(x=lon, y=lat)) +
#'  ggplot2::geom_tile(ggplot2::aes(fill = value),colour = "white") +
#'  ggplot2::scale_fill_gradient(low = "yellow", high = "red") +
#'  ggplot2::geom_point(data=Rio_metadata$PWSmetadata,col='black')
#'
GP_fit <- function(data.geo,...){
  if(class(data.geo)!="geodata") stop("First argument must be of class 'geodata'.")
  grid.positions <- create_grid(data.geo,...)
  fit <- geoR::ksline(data.geo, cov.model="exp",cov.pars=c(10,3), nugget=0,locations=grid.positions)
  grid.values <- as.data.frame(cbind(grid.positions,fit$predict))
  colnames(grid.values) <- c(colnames(grid.positions),"value")
  grid.values
}


##
## End zohren code
##
