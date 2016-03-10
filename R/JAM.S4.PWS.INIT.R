library(RCurl); library(RJSONIO); library(sp)

construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

gGeoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- construct.geocode.url(address)
  doc <- getURL(u)
  x <- RJSONIO::fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    return(c(lat, lng))
  } else {
    return(c(NA,NA))
  }
}

## EXAMPLES 
# samo_coords <- gGeoCode("Santa Cruz, CA")
# charlotte_coords <- gGeoCode("Charlotte, NC")

setClass(
  Class = "PWS.locations",
  slots = c(spatialPtDF="SpatialPointsDataFrame", spatialPt="SpatialPoints", call="list")
)

PWS.locations <- function(...) return(new(Class="PWS.locations",...))

setMethod("initialize",
          "PWS.locations",
          function(.Object, longitude, latitude, radius, user.key){
            args.length <- length(as.list(match.call()[-1]))
            if ( is.numeric(longitude) & (args.length == 5) ) {
              Meta.DF <- PWS_meta_query(longitude, latitude, radius, user.key)
            }else{
              location <- longitude
              g <- gGeoCode(location)
              Meta.DF <-PWS_meta_query(g[2], g[1], radius, user.key) 
            }
            coords <- cbind(Meta.DF$PWSmetadata$lon, Meta.DF$PWSmetadata$lat)
            .Object@spatialPt <- SpatialPoints(coords)
            .Object@spatialPtDF <- SpatialPointsDataFrame(.Object@spatialPt, Meta.DF$PWSmetadata)
            .Object@call <- Meta.DF$call
            return(.Object)
          }
)

###
## THESE LOCATIONS SHOULD TEST IDENTICAL
##
a <- PWS.locations("Santa Monica, CA", radius=3, user.key=user.key)

a
metadf.test <- PWS_meta_query(-118, 34, radius=3, user.key) 

metadf.test$PWSmetadata$id

a@spatialPtDF$id

class(a@spatialPtDF$)

PWS_conditions(list(PWSmetadata=a@spatialPtDF), user.key)
a <- PWS.locations("Santa Monica, CA", radius=3, user.key=user.key)
b <- PWS.locations(-118.49119, 34.01945, radius=3, user.key=jam.key) 
b@spatialPt@bbox
