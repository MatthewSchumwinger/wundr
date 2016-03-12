
require(ggmap); require(sp); require(jsonlite); require(geosphere)
# source("api_functions.R")

##
##   PWS.CONDITIONS USES PWS.LOCATIONS' ID COLUMN
##   TO RETRIEVE WEATHER CONDITIONS TO BE STORED IN GLOBAL VARIABLE
##   THE STD.API VERSION IS DESIGNED FOR USERS WITH API USAGE LIMITATIONS
##
setClass(
  Class = "PWS.Conditions",
  slots = c(spatialPtDF="SpatialPointsDataFrame", spatialPt="SpatialPoints", call="list")
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

            if (longitude < -180 | longitude > 180) stop("Please note that longitude must be -/+180.")
            if (latitude < -90 | latitude > 90) stop("Please note that latitude must be -/+90.")
            if (radius<=0) stop("Please note that the search radius must be positive.")
            if (typeof(user.key)!="character") stop("Please note that the user.key must be of type character.")

            args.length <- length(as.list(match.call()[-1]))
            cat("\n Note: The combined PWS.locations and PWS.Conditions may require several minutes... \n")
            cat("\n and nearly 100 API calls for areas in excess of 50km... \n")
            readline(prompt="Please press [enter] to continue or [esc] to quit.\n \n")
            if ( is.numeric(longitude) & (args.length == 5) ) {
              Meta.DF <- PWS_meta_query(longitude, latitude, radius, user.key)
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
            .Object@spatialPtDF <- SpatialPointsDataFrame(.Object@spatialPt, mergeDF(Meta.DF, myenv$repository))
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
  Id.Vector <- PWS.MetaQuery$PWSmetadata$id
  timePWS(Id.Vector)
}

timePWS <- function(ID.Vector){
  rows.MQ <- length(ID.Vector)
  ids <- lapply(1:ceiling(rows.MQ/10), function(x) return(ID.Vector[(10*(x-1)+1):(10*x)]))
  sapply(1:ceiling(rows.MQ/10), function(x) {
    ids <- as.data.frame(ids[x])[ !is.na( as.data.frame(ids[x]) )]
    pullPWS(ids)
    if(x!=ceiling(rows.MQ/10)) {cat("Pausing for required one-minute API relief...\n"); invisible(Sys.sleep(60))}
  })
}

pullPWS <- function(ids){
  sapply(ids, function(i) {
    url.base <- "http://api.wunderground.com/api/"
    url.cond <- "/conditions/q/"
    tmp.list <- jsonlite::fromJSON(paste0(url.base,user.key,url.cond,"pws:", i,".json"))$current_observation
    tmp.list$image <-NULL; tmp.list$observation_location <-NULL
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

##
##  EXAMPLE OF STANDALONE FUNCTIONS
# ##
# PWS.MetaQuery <- PWS_meta_query(-118.4912, 34.01945, 3, user.key)
# conditionsFetch(PWS.MetaQuery,user.key)
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

