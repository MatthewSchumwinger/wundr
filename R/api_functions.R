
createCentroidTable <- function(longitude,latitude,radius,max_radius_km) {
  centroidTable = matrix(c(longitude,latitude),ncol=2,byrow=TRUE)
  centroid_dist = floor(max_radius_km*sqrt(2)) 
  if(radius > 40){
  n = ceiling(radius/centroid_dist-0.5)
    for(i in -n:n){
      for(j in -n:n){
        exclude = (sqrt((i-0.5)^2+(j-0.5)^2)*centroid_dist > radius) &
          (sqrt((i-0.5)^2+(j+0.5)^2)*centroid_dist > radius) &
          (sqrt((i+0.5)^2+(j-0.5)^2)*centroid_dist > radius) &
          (sqrt((i+0.5)^2+(j+0.5)^2)*centroid_dist > radius)
        if( !exclude & !((i==0)&(j==0)) ) {
         centroidTable = rbind(centroidTable,
                              destPoint(destPoint(c(longitude,latitude),
                                                  90+90-sign(j)*90,abs(j)*centroid_dist*1e3),
                                        90-sign(i)*90,abs(i)*centroid_dist*1e3))
        }
      }
    }
  }
  centroidTable
}




PWS_meta_query  <- function(longitude, latitude, radius, user_key , 
                            km_miles = TRUE, stdAPI = TRUE){
  #  
  # Constants:
  url_base <- "http://api.wunderground.com/api/"
  url_geo <- "/geolookup/q/"
  max_radius_km = 40
  mile_per_km = 0.621371192237
  
  # Error tests on inputs:
  if(radius<=0) stop("Radius must be positive.")
  if((longitude < -180) | (longitude > 180) | (latitude < -90) | (latitude > 90)) 
    stop("Longitude must be in range -/+180 and latitude be in range -/+90.")
  if(typeof(user_key)!="character") stop("User key must be of type character.")
  
  # The function internally uses km, if radius give in miles, convert:
  if(!km_miles) radius = radius/mile_per_km
  
  # The API is restricted to searches over a radius of 40km
  # In case the radius is bigger than 40km, we cover the circle with smaller circles
  # The following loop determines the centroids of those circles:
  centroidTable <- createCentroidTable(longitude,latitude,radius,max_radius_km)
  
  
  cat("A total of ",nrow(centroidTable)," API calls is needed to download the metadata.\n")
  if(stdAPI) cat("Under standard API settings only 10 calls per minute are allowed.\n")
  cat("Downloading ")
  queries <-NULL
  count = 0 
  for(i in 1:nrow(centroidTable)){ 
    if(stdAPI & ( count%% 10 ==0 ) & count !=0) {
      cat(" Pausing ")
      Sys.sleep(60)
    }
    req <- fromJSON(paste0(url_base,user_key,
                           url_geo,centroidTable[i,2],",",centroidTable[i,1],".json"))
    # Check JSON for error, i.e. wrong key etc.
    if(!is.null(req$response$error)) stop(paste("JSON error:",req$response$error$description))
    
    queries=rbind(queries,req$location$nearby_weather_stations$pws$station)
    cat(".")
    count = count + 1
  } 
  cat(" Post-processing. ")
  if(is.null(queries)){
    # exit
    stop("No search results. Revise search parameter and user key.")
  }
  else {
    queries <- queries[!duplicated(queries$id),]
    queries$distance_km <- spDistsN1(as.matrix(queries[c("lon","lat")]), 
                                   c(longitude,latitude),longlat = TRUE)
    queries <- queries[queries$distance_km < radius,]
    queries$distance_mi <- queries$distance_km*mile_per_km
    queries <- queries[order(queries$distance_km),]
    cat("Done.")
    call=list("lon"=longitude,"lat"=latitude, "radius_km" = radius)
    list("PWSmetadata" = queries, "call" = call)
  }
}


# 
# Example usage:
# stefan.key <- XXXXXXXXXXXXXXXXX
# pwsmetadata <- PWS_meta_query(-122, 37, 50, stefan.key)
# head(pwsmetadata$PWSmetadata)
#





PWS_meta_subset  <- function(PWSmetadata,longitude, latitude, radius,
                             km_miles = TRUE){
  #  
  # Constants:
  mile_per_km = 0.621371192237  
  
  # Error tests on inputs:
  if(is.null(PWSmetadata$PWSmetadata)) stop("Provide a valid meta data object.")
  if(radius<=0) stop("Radius must be positive.")
  if((longitude < -180) | (longitude > 180) | (latitude < -90) | (latitude > 90)) 
    stop("Longitude must be in range -/+180 and latitude be in range -/+90.")
  
  # The function internally uses km, if radius give in miles, convert:
  if(!km_miles) radius = radius/mile_per_km
  
  queries <- PWSmetadata$PWSmetadata
  
  if(is.null(queries)){
    stop("Provide a valid meta data object.")
  }
  else {
    queries$distance_km <- spDistsN1(as.matrix(queries[c("lon","lat")]), 
                                   c(longitude,latitude),longlat = TRUE)
    queries <- queries[queries$distance_km < radius,]
    queries$distance_mi <- queries$distance_km*mile_per_km
    queries <- queries[order(queries$distance_km),]
    if(nrow(queries)==0) queries <- NULL
    call=list("lon"=longitude,"lat"=latitude, "radius_km" = radius)
    list("PWSmetadata" = queries, "call" = call)
  }
}




# 
# Example usage:
# pwsmetadata2 <- PWS_meta_subset(pwsmetadata,-122, 37, 2)
# pwsmetadata2$PWSmetadata
#





PWS_conditions  <- function(PWSmetadata,user_key , 
                            stdAPI = TRUE){
  #  
  # Constants:
  url_base <- "http://api.wunderground.com/api/"
  url_cond <- "/conditions/q/"
  
  # Error tests on inputs:
  if(is.null(PWSmetadata$PWSmetadata)) stop("Provide a valid meta data object.")
  if(typeof(user_key)!="character") stop("User key must be of type character.")

  cat("A total of ",nrow(PWSmetadata$PWSmetadata),
      " API calls is needed to download the metadata.\n")
  if(stdAPI) cat("Under standard API settings only 10 calls per minute are allowed.\n")
  cat("Downloading ")
  conditions <- NULL
  count = 0
  for(i in 1:nrow(PWSmetadata$PWSmetadata)){
    if(stdAPI & ( count%% 10 ==0 ) & count !=0) {
      cat(" Pausing ")
      Sys.sleep(60)
    }
    tmp.list <- fromJSON(paste0(url_base,user_key,url_cond,"pws:",PWSmetadata$PWSmetadata$id[i] ,".json"))
    # Check JSON for error, i.e. wrong key etc.
    if(!is.null(tmp.list$response$error)) stop(paste("JSON error:",tmp.list$response$error$description))
    #
    tmp.list$current_observation$image <-NULL
    tmp.list$current_observation$observation_location <-NULL
    conditions <- rbind(conditions,c(unlist(tmp.list$current_observation$display_location),
                                     unlist(tmp.list$current_observation)))
    cat(".")
    count = count + 1
  }
  
  conditions <- as.data.frame(conditions,stringsAsFactors=FALSE)
  rownames(conditions) <- PWSmetadata$PWSmetadata$id
  numCheck <- apply(conditions,2,function(x) suppressWarnings(all(!is.na(as.numeric(x)))))
  for(i in 1:ncol(conditions))
    if(numCheck[i]) conditions[,i] <- as.numeric(conditions[,i])
  
  cat("Done.")
  conditions
}



# 
# Example usage:
# cond <- PWS_conditions(pwsmetadata2,stefan.key)
# cond
#










PWS_history  <- function(PWSmetadata,begin_YYYYMMDD,end_YYYYMMDD,user_key , 
                            stdAPI = TRUE){
  #  
  # Constants:
  url_base <- "http://api.wunderground.com/api/"
  
  # Error tests on inputs:
  if(is.null(PWSmetadata$PWSmetadata)) stop("Provide a valid meta data object.")
  if(typeof(user_key)!="character") stop("User key must be of type character.")
    
  history <- NULL
  count = 0
  date_list <- tryCatch(seq(as.Date(begin_YYYYMMDD,"%Y%m%d"), 
                           as.Date(end_YYYYMMDD,"%Y%m%d"), by="days"),
           error = function(e) {stop("Dates must be in format 'YYYYMMDD' and in chronological order.")})
  
  date_list = gsub("-","",date_list)
  cat("A total of ",nrow(PWSmetadata$PWSmetadata)*length(date_list),
      " API calls is needed to download the metadata.\n")
  if(stdAPI) cat("Under standard API settings only 10 calls per minute are allowed.\n")
  
  cat("Downloading ")
  
  for(date in date_list){
    for(i in 1:nrow(PWSmetadata$PWSmetadata)){
      if(stdAPI & ( count%% 10 ==0 ) & count !=0) {
       cat(" Pausing ")
       Sys.sleep(60)
      }

      tmp.list <- fromJSON(paste0(url_base,user_key,"/history_",date,"/q/pws:",
                                                          PWSmetadata$PWSmetadata$id[i] ,".json"))
      # Check JSON for error, i.e. wrong key etc.
      if(!is.null(tmp.list$response$error)) stop(paste("JSON error:",tmp.list$response$error$description))
      #
      tmp.data <- tmp.list$history$observations$date   
      if(!is.null(tmp.data)) {
        tmp.data$id <- rep(PWSmetadata$PWSmetadata$id[i],nrow(tmp.data))
        tmp.data$latitude <- rep(PWSmetadata$PWSmetadata$lat[i],nrow(tmp.data))
        tmp.data$longitude <- rep(PWSmetadata$PWSmetadata$lon[i],nrow(tmp.data))
        tmp.list$history$observations$date<-NULL
        tmp.list$history$observations$utcdate<-NULL
        tmp.data <- cbind(tmp.data, as.data.frame(tmp.list$history$observations,stringsAsFactors=FALSE))
        history <- rbind(history,tmp.data)
      }
      cat(".")
      count = count + 1
    }
  }

  if(!is.null(history)){
    numCheck <- apply(history,2,function(x) suppressWarnings(all(!is.na(as.numeric(x)))))
    for(i in 1:ncol(history))
      if(numCheck[i]) history[,i] <- as.numeric(history[,i])
  }
  cat("Done.")
  history
}


# 
# Example usage:
# hist <- PWS_history(pwsmetadata2,"20160101","20160101",stefan.key) 
# View(hist)
#

