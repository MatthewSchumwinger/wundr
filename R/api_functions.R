# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# + This document contain the low level API functions which downlaod data from Weather Undergroound +
# + Those API functions form Part 1 of the projects and are used in later parts of the project by   +
# + by eaither calling them directly or incorporating parts of them. While the user is supposed to  +
# + interact with those functions through the S4 class, we have sufficiently documented the func-   +
# + tions, making it possible to use them indepentently. The functions include:                     +
# +                                                                                                 +
# + o createCentroidTable                                                                           +
# + o PWS_meta_query                                                                                +
# + o PWS_meta_subset                                                                               +
# + o PWS_conditions                                                                                +
# + o PWS_history                                                                                   +
# +                                                                                                 +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# require(jsonlite); require(sp); require(geosphere);

#' createCentroidTable
#'
#' createCentroidTable is an auxiliary function used in the 'PWS_meta_query' function.
#' Given a the coordinates of the centre of a large circle and its radius,
#' the createCentroidTable function creates a data frame of the (longitude,latitude)-pairs
#' for the centres of the small circles of a given radius which cover the large circle.
#'
#' @importFrom geosphere destPoint
#'
#' @param longitude Standard Longitude in range -/+180
#' @param latitude Standard latitude in range -/+90
#' @param radius Radius in kilometers of the large circle to be covered by small cricles
#' @param max_radius_km Radius in kilometers of the small cricles (e.g. 40km)
#' @return A data frame of the (longitude,latitude) pairs for the centres of the small circles which cover the large circle
#' @export
#' @examples
#' centre_table <- createCentroidTable(-122,37,400,40)
#' plot(centre_table,pch=18)
#'
createCentroidTable <- function(longitude,latitude,radius,max_radius_km) {
  # We cover the big circle with small circles of radius 'max_radius_km' (starndard 40km)
  # of minimal overlap. The centres of those circles are 'centroid_dist' apart:
  centroid_dist = floor(max_radius_km*sqrt(2))
  # discrete_width is the discrete diameter of the large circle(in units of diameters of small circles)
  discrete_width = ceiling(2*radius/centroid_dist)
  # We have to distinguish the two cases where the number is odd,
  # in which case we have a small cricle at the centre,
  # and where it is even (please refer to the figures in the vignette for details.)
  #
  # We place on small circle at the centre
  centroidTable = matrix(c(longitude,latitude),ncol=2,byrow=TRUE)
  #
  # First do teh case where discrete_width is odd:
  if( discrete_width %% 2 ==1) {
    n = floor(discrete_width/2)
    # DEBUG: cat('n',n)
    # now we take all the small circles with centres on a square-grid (-n,n)x(-n,n)
    # and remove the ones which are entriely outside the large circle
    for(i in -n:n){
      for(j in -n:n){
        exclude = (sqrt((i-0.5)^2+(j-0.5)^2)*centroid_dist > radius) &
          (sqrt((i-0.5)^2+(j+0.5)^2)*centroid_dist > radius) &
          (sqrt((i+0.5)^2+(j-0.5)^2)*centroid_dist > radius) &
          (sqrt((i+0.5)^2+(j+0.5)^2)*centroid_dist > radius)
        if( !exclude & !((i==0)&(j==0))) {
          centroidTable = rbind(centroidTable,
                                geosphere::destPoint(geosphere::destPoint(c(longitude,latitude),
                                                                          90+90-sign(j)*90,abs(j)*centroid_dist*1e3),
                                                     90-sign(i)*90,abs(i)*centroid_dist*1e3))
        }
      }
    }
  }
  #
  # Next the case where discrete_width is even:
  else {
    n = discrete_width/2
    # DEBUG: cat('n',n)
    # now we take all the small circles with centres on a square-grid (n+0.5,n-0.5)x(n+0.5,n-0.5)
    # and remove the ones which are entriely outside the large circle
    range <- -n:n
    range = range[range!=0]
    for(i in range){
      for(j in range){
        # shift the grid from (-n,n)x(-n,n) to (n+0.5,n-0.5)x(n+0.5,n-0.5)
        i_shift = sign(i)*(abs(i)-0.5)
        j_shift = sign(j)*(abs(j)-0.5)
        exclude = (sqrt((i_shift-0.5)^2+(j_shift-0.5)^2)*centroid_dist > radius) &
          (sqrt((i_shift-0.5)^2+(j_shift+0.5)^2)*centroid_dist > radius) &
          (sqrt((i_shift+0.5)^2+(j_shift-0.5)^2)*centroid_dist > radius) &
          (sqrt((i_shift+0.5)^2+(j_shift+0.5)^2)*centroid_dist > radius)
        if( !exclude ) {
          centroidTable = rbind(centroidTable,
                                geosphere::destPoint(geosphere::destPoint(c(longitude,latitude),
                                                                          90+90-sign(j_shift)*90,(abs(j_shift))*centroid_dist*1e3),
                                                     90-sign(i_shift)*90,(abs(i_shift))*centroid_dist*1e3))
        }
      }
    }
  }
  centroidTable
}








#' PWS_meta_query
#'
#' Given a the coordinates of the centre of a circle and its radius the 'PWS_meta_query'
#' function downloads the meta data for all personal weather stations in this region from
#' WUnderground. Due to the limitations of the API this is done by covering the larger
#' circle with small circles, each of which corresponds to a single API call. This is done
#' using the auxiliary function createCentroidTable. While downloading the action of each
#' download is displayed by a '.' If the function is used under standard API settings, only
#' 10 API calls per minute are allowed and the function pauses accordingly. In some instances
#' the API call only returns partial data. In those cases, the 'PWS_meta_query' function
#' zooms in, issuing more queries around this area to capture all stations. Those additional
#' API calls are denoted by ','
#'
#' @importFrom jsonlite fromJSON
#' @importFrom geosphere destPoint
#' @importFrom sp spDistsN1
#'
#' @param longitude Standard Longitude in range -/+180
#' @param latitude Standard latitude in range -/+90
#' @param radius Radius in kilometers of the circle
#' @param user_key Your WUnderground API user key given as a character string
#' @param km_miles A bolean variable indicating whether radius is in kilometer (TRUE) or miles (FALSE). The default is kilometers.
#' @param stdAPI A bolean variable indicating whether you have a standard (free) API access (TRUE) which only allows for 10 API calls per minute. The default is the standard API.
#' @return A list of two entries. The first has information on the original arguments of the function (radius and coordinates of circle). The second entry contains a data frame with all personal weather stations in the region of the circle.
#' @export
#' @examples
#' # your.key <- "xxxxxxxxxx" # replace this with your key
#' # pwsmetadata <- PWS_meta_query(-122, 37, 50, your.key)
#' # if you run the above code with your key the output should be the same as provided here:
#' # TODO ++++ SAVE DATA AND LOAD IT HERE
#' # head(pwsmetadata)
#'
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
  if(radius > max_radius_km) centroidTable <- createCentroidTable(longitude,latitude,radius,max_radius_km)
  else centroidTable = matrix(c(longitude,latitude),ncol=2,byrow=TRUE)

  cat("A minimum of ",nrow(centroidTable)," API calls is needed to download the metadata.\n")
  if(stdAPI) cat("Under standard API settings only 10 calls per minute are allowed.\n")
  cat("(API calls are denoted by '.' If WUnderground return only partial data,\n")
  cat("new additional API calls are made. Leading to more than ",nrow(centroidTable)," calls.\n")
  cat("Those new calls are denoted by ',')\n")
  cat("Downloading ")
  queries <-NULL
  count = 0
  for(i in 1:nrow(centroidTable)){
    if(stdAPI & ( count%% 10 ==0 ) & count !=0) {
      cat(" Pausing ")
      Sys.sleep(60)
    }
    req <- jsonlite::fromJSON(paste0(url_base,user_key,
                                     url_geo,centroidTable[i,2],",",centroidTable[i,1],".json"))
    # Check JSON for error, i.e. wrong key etc.
    if(!is.null(req$response$error)) stop(paste("JSON error:",req$response$error$description))
    queries=rbind(queries,req$location$nearby_weather_stations$pws$station)
    count = count + 1
    cat(".")

    #DEBUG
    #cat("\n ", nrow(req$location$nearby_weather_stations$pws$station), " ", nrow(req$location$nearby_weather_stations$airport$station))
    #DEBUG
    #cat(" max dist:",max(req$location$nearby_weather_stations$pws$station$distance_km),"\n")

    # Zoom in:
    if(max(req$location$nearby_weather_stations$pws$station$distance_km)<25){
      for(ind_i in c(-1,1)){
        for(ind_j in c(-1,1)){
          vec <- geosphere::destPoint(geosphere::destPoint(centroidTable[i,],
                                                           90+90-sign(ind_j)*90,abs(ind_j)*max_radius_km*0.45*1e3),
                                      90-sign(ind_i)*90,abs(ind_i)*max_radius_km*0.45*1e3)
          #DEBUG:
          #points(vec[1],vec[2],col='blue')
          if(stdAPI & ( count%% 10 ==0 ) & count !=0) {
            cat(" Pausing ")
            Sys.sleep(60)
          }
          req <- jsonlite::fromJSON(paste0(url_base,user_key,
                                           url_geo,vec[2],",",vec[1],".json"))
          if(!is.null(req$response$error)) stop(paste("JSON error:",req$response$error$description))
          queries=rbind(queries,req$location$nearby_weather_stations$pws$station)
          count = count + 1
          cat(",")
        }
      }
    }
  }
  cat(" Post-processing. ")
  if(is.null(queries)) stop("No search results. Revise search parameters.")

  queries <- queries[!duplicated(queries$id),]
  queries$distance_km <- sp::spDistsN1(as.matrix(queries[c("lon","lat")]),
                                       c(longitude,latitude),longlat = TRUE)
  queries <- queries[queries$distance_km < radius,]
  queries$distance_mi <- queries$distance_km*mile_per_km
  queries <- queries[order(queries$distance_km),]
  cat("Done.")
  call=list("lon"=longitude,"lat"=latitude, "radius_km" = radius)
  list("PWSmetadata" = queries, "call" = call)
}








#' PWS_meta_subset
#'
#' Given an output of 'PWS_meta_query' which contains meta data of personal weather stations in a region
#' and the coordinates of the centre of a (new) circle and its radius, the 'PWS_meta_subset' function
#' subsets the meta data to only cotain stations inside the new circle. The output is the subsetted meta data.
#'
#' @importFrom sp spDistsN1
#'
#' @param PWSmetadata Meta data object of weather stations (output of 'PWS_meta_query')
#' @param longitude Standard Longitude in range -/+180
#' @param latitude Standard latitude in range -/+90
#' @param radius Radius in kilometers of the circle
#' @param km_miles A bolean variable indicating whether radius is in kilometer (TRUE) or miles (FALSE). The default is kilometers.
#' @return A list of two entries. The first has information on the original arguments of the function (radius and coordinates of circle). The second entry contains a subsetted data frame with all personal weather stations in the region of the circle.
#' @export
#' @examples
#' # TODO LOAD DATA +++++
#' # pwsmetadata_sub <- PWS_meta_subset(pwsmetadata,-122, 37, 2)
#' # pwsmetadata_sub
#'
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

  if(is.null(queries)) stop("Provide a valid meta data object.")


  queries$distance_km <- sp::spDistsN1(as.matrix(queries[c("lon","lat")]),
                                       c(longitude,latitude),longlat = TRUE)
  queries <- queries[queries$distance_km < radius,]
  queries$distance_mi <- queries$distance_km*mile_per_km
  queries <- queries[order(queries$distance_km),]
  if(nrow(queries)==0) queries <- NULL
  call=list("lon"=longitude,"lat"=latitude, "radius_km" = radius)
  list("PWSmetadata" = queries, "call" = call)
}








#' PWS_conditions
#'
#' Given an output of 'PWS_meta_query' which contains meta data of personal weather stations in a region
#' the function 'PWS_conditions' downloads the weather conditions for all stations in the meta data.
#'
#'
#' @importFrom jsonlite fromJSON
#'
#' @param PWSmetadata Meta data object of weather stations (output of 'PWS_meta_query')
#' @param user_key Your WUnderground API user key given as a character string
#' @param stdAPI A bolean variable indicating whether you have a standard (free) API access (TRUE) which only allows for 10 API calls per minute. The default is the standard API.
#' @return A data frame containing all the weather conditions of the stations in the provided meta data object.
#' @export
#' @examples
#' #' # your.key <- "xxxxxxxxxx" # replace this with your key
#' # cond <- PWS_conditions(pwsmetadata2,stefan.key)
#' # if you run the above code with your key the output should be the same as provided here:
#' # TODO ++++ SAVE DATA AND LOAD IT HERE
#' # head(cond)
#'
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
    tmp.list <- jsonlite::fromJSON(paste0(url_base,user_key,url_cond,"pws:",PWSmetadata$PWSmetadata$id[i] ,".json"))
    # Check JSON for error, i.e. wrong key etc.
    if(!is.null(tmp.list$response$error)) stop(paste("JSON error:",tmp.list$response$error$description))
    else{
      tmp.list$current_observation$image <-NULL
      tmp.list$current_observation$observation_location <-NULL
      conditions <- rbind(conditions,c(unlist(tmp.list$current_observation$display_location),
                                       unlist(tmp.list$current_observation)))
    }
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












#' PWS_history
#'
#' Given an output of 'PWS_meta_query' which contains meta data of personal weather stations in a region
#' and a start and an end date for a period of time, the function 'PWS_history' downloads the weather
#' conditions history for all stations in the meta data. Note that 1 API per stations per day is required.
#'
#'
#' @importFrom jsonlite fromJSON
#'
#' @param PWSmetadata Meta data object of weather stations (output of 'PWS_meta_query')
#' @param begin_YYYYMMDD
#' @param end_YYYYMMDD
#' @param user_key Your WUnderground API user key given as a character string
#' @param stdAPI A bolean variable indicating whether you have a standard (free) API access (TRUE) which only allows for 10 API calls per minute. The default is the standard API.
#' @return A data frame containing all the weather conditions of the stations in the provided meta data object.
#' @export
#' @examples
#' # your.key <- "xxxxxxxxxx" # replace this with your key
#' # hist <- PWS_history(pwsmetadata,"20160101","20160101",your.key)
#' # if you run the above code with your key the output should be the same as provided here:
#' # TODO ++++ SAVE DATA AND LOAD IT HERE
#' # head(hist)
#'
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

      tmp.list <- jsonlite::fromJSON(paste0(url_base,user_key,"/history_",date,"/q/pws:",
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



















