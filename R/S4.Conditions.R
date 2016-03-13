require(jsonlite); require(httr)
#' An S4 class to represent a bank account.
#' @include S4.Locations.R
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sp SpatialPoints
#' @export
#' @slot spatialPtDF A length-one numeric vector
#' @slot spatialPt A legnth-one numeric vector
#' @slot call list of stuff
#' @slot data
#'
setClass(
  Class = "PWS.Conditions",
  slots = c(spatialPtDF="SpatialPointsDataFrame", spatialPt="SpatialPoints", call="list", data="data.frame")
)
##
##  THE USER SIMULTANEOUSLY GAINS ACCESS TO A GLOBALLY AVAILABLE DATAFRAME
##  AS WELL AS SPATIALPOINTS AND A SPATIALPOINTSDATAFRAME
##
setClass(
  Class="conds",
  slots = c(id = "character", data = "data.frame")
)
##
##  THE SLOTS REFLECT THE REALITY OF THE USER AND DEVELOPER PROFILE
##  SIMULTANEOUSLY IMMEDIATE WEATHER AND GEOSPATIAL DATA
##
PWS.Conditions <- function(...) return(new(Class="PWS.Conditions",...))
##
##  THE INITIALIZATION FUNCTION SIMULTANEOUSLY RETRIEVES AND FORMATS SLOT DATA
##
myenv <- new.env(parent=emptyenv())
myenv$repository = list()
setMethod("initialize",
          "PWS.Conditions",
          function(.Object, longitude, latitude, radius, user.key, STD.API=TRUE){

            if (radius<=0) stop("Please note that the search radius must be positive.")
            if (typeof(user.key)!="character") stop("Please note that the user.key must be of type character.")

            args.length <- length(as.list(match.call()[-1]))
            cat("\n Note: The combined PWS.locations and PWS.Conditions may require several minutes... \n")
            cat("\n and nearly 100 API calls for areas in excess of 50km... \n")
            readline(prompt="Please press [enter] to continue or [esc] to quit.\n \n")


            if ( is.numeric(longitude) & (args.length == 5) ) {
              Meta.DF <- PWS_meta_query(longitude, latitude, radius, user_key=user.key)
              if (longitude < -180 | longitude > 180) stop("Please note that longitude must be -/+180.")
              if (latitude < -90 | latitude > 90) stop("Please note that latitude must be -/+90.")
            }else{
              location <- longitude
              g <- j.geocode(location)
              Meta.DF <-PWS_meta_query(longitude=g[2], latitude=g[1], radius=radius, user_key=user.key)
            }
            coords <- cbind(Meta.DF$PWSmetadata$lon, Meta.DF$PWSmetadata$lat)
            .Object@spatialPt <- SpatialPoints(coords)
            .Object@call <- Meta.DF$call

            if (STD.API==TRUE) {
            conditionsFetch(Meta.DF, user.key)
            cat("\n Please wait 60 seconds so as not to exceed your API limit... \n"); Sys.sleep(60)

            merged <- mergeDF(Meta.DF, myenv$repository)

            logical.vector <- sapply(merged$relative_humidity, function(x) is.null(x))
            merged$relative_humidity[logical.vector] <- NA

            logical.vector <- sapply(merged$wind_string, function(x) is.null(x))
            merged$wind_string[logical.vector] <- NA

            logical.vector <- sapply(merged$wind_dir, function(x) is.null(x))
            merged$wind_dir[logical.vector] <- NA

            logical.vector <- sapply(merged$wind_degrees, function(x) is.null(x))
            merged$wind_degrees[logical.vector] <- NA

            logical.vector <- sapply(merged$wind_mph, function(x) is.null(x))
            merged$wind_mph[logical.vector] <- NA

            logical.vector <- sapply(merged$wind_gust_mph, function(x) is.null(x))
            merged$wind_gust_mph[logical.vector] <- NA

            logical.vector <- sapply(merged$wind_kph, function(x) is.null(x))
            merged$wind_kph[logical.vector] <- NA

            logical.vector <- sapply(merged$wind_gust_kph, function(x) is.null(x))
            merged$wind_gust_kph[logical.vector] <- NA

            logical.vector <- sapply(merged$pressure_mb, function(x) is.null(x))
            merged$pressure_mb[logical.vector] <- NA

            logical.vector <- sapply(merged$pressure_in, function(x) is.null(x))
            merged$pressure_in[logical.vector] <- NA

            logical.vector <- sapply(merged$pressure_trend, function(x) is.null(x))
            merged$pressure_trend[logical.vector] <- NA

            logical.vector <- sapply(merged$dewpoint_string, function(x) is.null(x))
            merged$dewpoint_string[logical.vector] <- NA

            logical.vector <- sapply(merged$dewpoint_c, function(x) is.null(x))
            merged$dewpoint_c[logical.vector] <- NA

            logical.vector <- sapply(merged$dewpoint_f, function(x) is.null(x))
            merged$dewpoint_f[logical.vector] <- NA

            logical.vector <- sapply(merged$heat_index_string, function(x) is.null(x))
            merged$heat_index_string[logical.vector] <- NA

            logical.vector <- sapply(merged$heat_index_c, function(x) is.null(x))
            merged$heat_index_c[logical.vector] <- NA

            logical.vector <- sapply(merged$heat_index_f, function(x) is.null(x))
            merged$heat_index_f[logical.vector] <- NA

            logical.vector <- sapply(merged$windchill_string, function(x) is.null(x))
            merged$windchill_string[logical.vector] <- NA

            logical.vector <- sapply(merged$windchill_c, function(x) is.null(x))
            merged$windchill_c[logical.vector] <- NA

            logical.vector <- sapply(merged$windchill_f, function(x) is.null(x))
            merged$windchill_f[logical.vector] <- NA

            logical.vector <- sapply(merged$feelslike_string, function(x) is.null(x))
            merged$feelslike_string[logical.vector] <- NA

            logical.vector <- sapply(merged$feelslike_f, function(x) is.null(x))
            merged$feelslike_f[logical.vector] <- NA

            logical.vector <- sapply(merged$feelslike_c, function(x) is.null(x))
            merged$feelslike_c[logical.vector] <- NA

            logical.vector <- sapply(merged$visibility_mi, function(x) is.null(x))
            merged$visibility_mi[logical.vector] <- NA

            logical.vector <- sapply(merged$visibility_km, function(x) is.null(x))
            merged$visibility_km[logical.vector] <- NA

            logical.vector <- sapply(merged$solarradiation, function(x) is.null(x))
            merged$solarradiation[logical.vector] <- NA

            logical.vector <- sapply(merged$UV, function(x) is.null(x))
            merged$UV[logical.vector] <- NA

            logical.vector <- sapply(merged$precip_1hr_string, function(x) is.null(x))
            merged$precip_1hr_string[logical.vector] <- NA

            logical.vector <- sapply(merged$precip_1hr_metric, function(x) is.null(x))
            merged$precip_1hr_metric[logical.vector] <- NA

            logical.vector <- sapply(merged$precip_today_string, function(x) is.null(x))
            merged$precip_today_string[logical.vector] <- NA

            logical.vector <- sapply(merged$precip_today_in, function(x) is.null(x))
            merged$precip_today_in[logical.vector] <- NA

            logical.vector <- sapply(merged$precip_today_metric, function(x) is.null(x))
            merged$precip_today_metric[logical.vector] <- NA

            logical.vector <- sapply(merged$icon, function(x) is.null(x))
            merged$icon[logical.vector] <- NA

            logical.vector <- sapply(merged$precip_1hr_in, function(x) is.null(x))
            merged$precip_1hr_in[logical.vector] <- NA


            .Object@spatialPtDF <- SpatialPointsDataFrame(.Object@spatialPt, merged)

            n <- nrow(.Object@spatialPtDF@data)

            names.col <- colnames(.Object@spatialPtDF@data)

            df <- matrix(unlist(.Object@spatialPtDF@data), nrow=n)

            df.DF <- as.data.frame(df,stringsAsFactors = FALSE )

            colnames(df.DF) <- names.col

            .Object@data <- df.DF

            }else{
            .Object@spatialPtDF <- SpatialPointsDataFrame(.Object@spatialPt, conditionsFetch.Full(Meta.DF, user.key))
            }

            return(.Object)
          }
)
##
## NOTE: CONDITIONS FETCH DOES NOT RETURN DATA DIRECTLY,
## BUT INSTEAD SENDS IT THE MYENV$REPOSITORY WHICH GETS MERGED
## INTO A SPATIALPOINTSDATAFRAME
##
conditionsFetch <- function(PWS.MetaQuery, user.key){
  user.key <- user.key
  Id.Vector <- PWS.MetaQuery$PWSmetadata$id
  list(Id.Vector, user.key)
  timePWS(Id.Vector, user.key)
}

<<<<<<< HEAD
#conditionsFetch(PWS.MetaQuery, user.key=jam.key)
=======
# conditionsFetch(PWS.MetaQuery, user.key=jam.key)
>>>>>>> origin/master

timePWS <- function(ID.Vector,user.key){
  rows.MQ <- length(ID.Vector)
  ids <- lapply(1:ceiling(rows.MQ/10), function(x) return(ID.Vector[(10*(x-1)+1):(10*x)]))
  sapply(1:ceiling(rows.MQ/10), function(x) {
    ids <- as.data.frame(ids[x])[ !is.na( as.data.frame(ids[x]) )]
    pullPWS(ids,user.key)
    if(x!=ceiling(rows.MQ/10)) {cat("\n Pausing for required one-minute API relief...\n"); invisible(Sys.sleep(60))}
  })
}

pullPWS <- function(ids,user.key){
  sapply(ids, function(i) {
    url.base <- "http://api.wunderground.com/api/"
    url.cond <- "/conditions/q/"
    tmp.list <- jsonlite::fromJSON(paste0(url.base,user.key,url.cond,"pws:", i,".json"))$current_observation
    tmp.list$image <-NULL; tmp.list$observation_location <-NULL;
    tmp.list$display_location <-NULL; tmp.list$estimated <-NULL;
    tmp.list$observation_time <-NULL; tmp.list$observation_time_rfc822 <-NULL;
    tmp.list$observation_epoch <-as.character(tmp.list$observation_epoch);
    tmp.list$local_time_rfc822 <- NULL;
    tmp.list$local_epoch <-as.character(tmp.list$local_epoch);
    tmp.list$icon_url<- NULL;
    tmp.list$forecast_url<- NULL;
    tmp.list$history_url <- NULL;
    tmp.list$ob_url <- NULL;
    tmp.list$nowcast <- NULL
    myenv$repository[[i]] <- tmp.list
  })
}

mergeDF <- function(PWS.MetaQuery,repository.List){
  df <- do.call(rbind, repository.List)
  Merged.JDF <- merge(PWS.MetaQuery$PWSmetadata, df , by.x=c("id"), by.y=c("station_id") )
  myenv$myConds <- new(Class="conds", id=Merged.JDF$id, data=Merged.JDF)
  return(Merged.JDF) #READY FOR TRANSFORMATION INTO SPATIALPOINTSDATAFRAME
}

conditionsFetch.Full <- function(PWS.MetaQuery, user.key){
  url.base <- "http://api.wunderground.com/api/"
  url.cond <- "/conditions/q/"
  Id.Vector <- PWS.MetaQuery$PWSmetadata$id
  PWS.json <-  as.data.frame(sapply(Id.Vector, function(i) jsonlite::fromJSON(paste0(url.base,user.key,url.cond,"pws:", i,".json"))$current_observation))
  Merged.JDF <- merge(PWS.MetaQuery$PWSmetadata, t(PWS.json), by.x=c("id"), by.y=c("station_id") )
  myenv$myConds <- new(Class="conds", id=Merged.JDF$id, data=Merged.JDF)
  return(Merged.JDF) #READY TO USE WITH SPATIALDATAFRAME CONSTRUCTION
}

###
## EXAMPLES
##
# S4.SpatialPDF2 <- PWS.Conditions("Santa Monica,CA", radius=3, user.key=jam.key)
# S4.SpatialPDF3 <- PWS.Conditions("Santa Monica,CA", radius=3, user.key=jam.key)


















#
# ##
# ##  EXAMPLE OF STANDALONE FUNCTIONS
# # ##
# PWS.MetaQuery <- PWS_meta_query(-118.4912, 34.01945, 3, user_key=jam.key)
# conditionsFetch(PWS.MetaQuery, user.key)
# #
# #   CREATION OF REPOSITORY LIST: myenv$repository
# #   REPOSITORY CONDITIONS LIST NEEDS TO BE TRANSFORMED TO DATAFRAME AND MERGED
# #   TO PWS.METAQUERY FOR SUBSEQUENT SPATIALPOINTSDATAFRAME TRANSFORMATION
# #
# merged.Conditions <- mergeDF(PWS.MetaQuery, myenv$repository)
# coords <- cbind(PWS.MetaQuery$PWSmetadata$lon, PWS.MetaQuery$PWSmetadata$lat)
# spatial.points <- SpatialPoints(coords)
# SpatialPDF <- SpatialPointsDataFrame(spatial.points, merged.Conditions)
# ##
# ##   EXAMPLE OF CLASS INITIALIZATION FUNCTION LEADING TO SPATIALPOINTSDATAFRAME SLOT
# ##   AS WELL AS CONDITIONS DATAFRAME
# ##
# S4.SpatialPDF <- PWS.Conditions(-118.4912, 34.01945, 3, user.key)
#
# identical(SpatialPDF@data, S4.SpatialPDF@spatialPtDF@data)
# #   NOTE - THEY ARE NOT IDENTICAL BECAUSE THEIR OBSERVATION TIMES WILL INEVITABLY DIFFER BY A FEW SECONDS
# identical(SpatialPDF@data$id, S4.SpatialPDF@spatialPtDF@data$id)
# identical(S4.SpatialPDF@spatialPtDF@data$id,myenv$myConds@data$id )

#   IDENTICAL - TRUE
#setConds(myenv$myConds)

