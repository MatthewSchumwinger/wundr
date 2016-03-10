require(ggmap); require(sp); require(RJSONIO); require(jsonlite); library(RCurl)
source("JAM_1.R"); source("api_functions.R") #LATEST VERSION LOCATED IN SAME FOLDER
##
##   PWS.CONDITIONS USES PWS.LOCATIONS' ID COLUMN 
##   TO RETRIEVE WEATHER CONDITIONS TO BE STORED IN GLOBAL VARIABLE
##   THE FULL.API VERSION IS DESIGNED FOR USERS WITH UNLIMITED API USAGE
##
setClass(
  Class = "PWS.conditions",
  slots = c(spatialPtDF="SpatialPointsDataFrame", spatialPt="SpatialPoints", call="list")
)
##
##  THE USER SIMULTANEOUSLY GAINS ACCESS TO A GLOBALLY AVAILABLE DATAFRAME 
##  AS WELL AS SPATIALPOINTS AND A SPATIALPOINTSDATAFRAME
##
setClass(Class="conds",
         representation = representation(
           id = "character",
           data = "data.frame"
         ))
##
##  THE SLOTS REFLECT THE REALITY OF THE USER AND DEVELOPER PROFILE
##  SIMULTANEOUSLY IMMEDIATE WEATHER AND GEOSPATIAL DATA
##
PWS.conditions <- function(...) return(new(Class="PWS.conditions",...))
##
##  THE INITIALIZATION FUNCTION SIMULTANEOUSLY RETRIEVES AND FORMATS SLOT DATA
##
setMethod("initialize",
          "PWS.conditions",
          function(.Object, longitude, latitude, radius, user.key){
            args.length <- length(as.list(match.call()[-1]))
            cat("\n Note: The combined PWS.locations and PWS.conditions may require several minutes... \n")
            cat("\n and nearly 100 API calls for areas in excess of 50km... \n")
            readline(prompt="Please press [enter] to continue or [esc] to quit.\n \n")
            if ( is.numeric(longitude) & (args.length == 5) ) {
              Meta.DF <- PWS_meta_query(longitude, latitude, radius, user.key)
            }else{
              location <- longitude
              g <- gGeoCode(location)
              Meta.DF <-PWS_meta_query(longitude=g[2], latitude=g[1], radius=radius, user_key=user.key) 
            }
            coords <- cbind(Meta.DF$PWSmetadata$lon, Meta.DF$PWSmetadata$lat)
            .Object@spatialPt <- SpatialPoints(coords)
            #cat("\n Please wait 60 seconds so as not to exceed your API limit... \n"); Sys.sleep(60)
            .Object@spatialPtDF <- SpatialPointsDataFrame(.Object@spatialPt, conditionsFetch(Meta.DF, user.key))
            .Object@call <- Meta.DF$call
            return(.Object)
          }
)
##
##   FETCHES THE CONDITIONS GIVEN THE ID NUMBERS 
##   RETRIEVED THROUGH THE PWS.LOCATIONS() META-QUERY
##
myenv <- new.env(parent=emptyenv())
conditionsFetch <- function(PWS.MetaQuery, user.key){
  url.base <- "http://api.wunderground.com/api/"
  url.cond <- "/conditions/q/"
  Id.Vector <- PWS.MetaQuery$PWSmetadata$id
  PWS.json <-  as.data.frame(sapply(Id.Vector, function(i) jsonlite::fromJSON(paste0(url.base,user.key,url.cond,"pws:", i,".json"))$current_observation)) 
  Merged.JDF <- merge(PWS.MetaQuery$PWSmetadata, t(PWS.json), by.x=c("id"), by.y=c("station_id") )
  myenv$myConds <- new(Class="conds", id=Merged.JDF$id, data=Merged.JDF)
  return(Merged.JDF) #READY TO USE WITH SPATIALDATAFRAME CONSTRUCTION
}
##
## EXAMPLE
##
Spatial.Conditions <- PWS.conditions("Santa Monica, CA", radius=2, user.key = user.key)
Spatial.Conditions@spatialPtDF