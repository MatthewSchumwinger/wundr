# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# + This document contains the low level API functions which downlaod data from Wheather Underground+
# + Those API functions form Part 1 of the projects and are used in later parts of the project      +
# + by either calling them directly or incorporating parts of them. While the user is supposed to   +
# + interact with those functions through the S4 class, we have sufficiently documented the func-   +
# + tions, making it possible to use them indepentently.                                            +
# +                                                                                                 +
# + The functions include:                                                                          +
# +                                                                                                 +
# + o createCentroidTable                                                                           +
# + o PWS_meta_query                                                                                +
# + o PWS_meta_subset                                                                               +
# + o PWS_conditions                                                                                +
# + o PWS_history                                                                                   +
# +                                                                                                 +
# + There are also data sets which are included and which show the output of those functions:       +
# +                                                                                                 +
# + o Rio_basemap                                                                                  +
# + o Rio_metadata                                                                                  +
# + o Rio_conditions                                                                                +
# + o Rio_history                                                                                   +
# +                                                                                                 +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +                                            FUNCTIONS                                            +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#
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
#' @importFrom ggmap ggmap
#' @importFrom ggplot2 ggplot geom_point
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
#' # The following command downloads the meta data for all stations in the Rio de Janeiro region:
#' # Rio_metadata <- PWS_meta_query(-43.185368,-22.856878, 50, your.key)
#' # if you run the above code with your key the output should be the same as provided here:
#' data(Rio_basemap)
#' data(Rio_metadata)
#' require(ggmap);require(ggplot2);
#' ggmap(Rio_basemap)+geom_point(data=Rio_metadata$PWSmetadata,col='red')
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
#' @importFrom ggmap ggmap
#' @importFrom ggplot2 ggplot geom_point
#'
#' @param PWSmetadata Meta data object of weather stations (output of 'PWS_meta_query')
#' @param longitude Standard Longitude in range -/+180
#' @param latitude Standard latitude in range -/+90
#' @param radius Radius in kilometers of the circle
#' @param km_miles A bolean variable indicating whether radius is in kilometer (TRUE) or miles (FALSE). The default is kilometers.
#' @return A list of two entries. The first has information on the original arguments of the function (radius and coordinates of circle). The second entry contains a subsetted data frame with all personal weather stations in the region of the circle.
#' @export
#' @examples
#' # First we load the output of the command
#' # Rio_metadata <- PWS_meta_query(-22.856878, -43.185368, 50, your.key)
#' # where our.key is your API-key for Weather Underground.
#' data(Rio_metadata)
#' # Now we subset:
#' Rio_centre_metadata <- PWS_meta_subset(Rio_metadata,-43.185368,-22.856878, 10)
#' # and plot it
#' data(Rio_basemap)
#' require(ggmap);require(ggplot2);
#' ggmap(Rio_basemap)+geom_point(data=Rio_centre_metadata$PWSmetadata,col='red')
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
#' # You can download the current conditions from waether stations in Rio de Janeiro using
#' # the command below if you replace 'your.key' with your API-key from Weather Underground:
#' # data(Rio_metadata)
#' # Rio_conditions <- PWS_conditions(Rio_metadata, your.key)
#' # if you run the above code with your key the output should be the same as provided here:
#' data(Rio_conditions)
#' head(Rio_conditions)
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
      conditions <- suppressWarnings(rbind(conditions,c(unlist(tmp.list$current_observation$display_location),
                                       unlist(tmp.list$current_observation))))
      # some stations have some additional url information at the end, since those differ
      # from station to station, the above might get a warning, which we ignor since we remove
      # this information below anyhow.
    }
    cat(".")
    count = count + 1
  }

  conditions <- as.data.frame(conditions,stringsAsFactors=FALSE)
  conditions$icon <- NULL
  conditions$icon_url <- NULL
  conditions$forecast_url <- NULL
  conditions$history_url <- NULL
  conditions$ob_url <- NULL
  conditions$nowcast <- NULL
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
#' @importFrom zoo zoo
#'
#' @param PWSmetadata Meta data object of weather stations (output of 'PWS_meta_query')
#' @param begin_YYYYMMDD Beginning date in format "YYYYMMDD"
#' @param end_YYYYMMDD Ending date in format "YYYYMMDD"
#' @param user_key Your WUnderground API user key given as a character string
#' @param stdAPI A bolean variable indicating whether you have a standard (free) API access (TRUE) which only allows for 10 API calls per minute. The default is the standard API.
#' @return A data frame containing all the weather conditions of the stations in the provided meta data object.
#' @export
#' @examples
#' # You can download the current conditions from weather stations in Rio de Janeiro using
#' # the command below if you replace 'your.key' with your API-key from Weather Underground:
#' # data(Rio_metadata)
#' # Rio_history <- PWS_history(Rio_metadata,"20151224","20151231",your.key)
#' # if you run the above code with your key the output should be the same as provided here:
#' data(Rio_history)
#' head(Rio_history)
#' hist.zoo <- history_zoo(Rio_history,"IRIODEJA53",c("hum","tempm"))
#' plot(hist.zoo,col='red', main = "Humidity and Temperatur")
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




# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +                                            DATA SETS                                            +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#' Rio_basemap object
#'
#' Object of tyoe ggmapraster containing the base map of Rio de Janiero. For visualisation
#' of example data sets. The base map can be plotted using 'ggmap'
#'
#' @importFrom ggmap ggmap
#' @examples
#' data(Rio_basemap)
#' require(ggmap)
#' ggmap(Rio_basemap)
#'
#' @author retrived from google maps
"Rio_basemap"




#' Rio_metadata dataset
#'
#' This is the meta_data for the Personal Weather Stations in Rio de Janeiro, Brazil.
#' It is the output of the following call of the 'PWS_meta_query' function, where
#' 'your.key' shoudl be replaced with your API-key for Weather Underground:
#' Rio_metadata <- PWS_meta_query(-22.856878, -43.185368, 50, your.key)
#'
#' @examples
#' data(Rio_metadata)
#' head(Rio_metadata$PWSmetadata)
#'
#' @author wundr team
"Rio_metadata"



#' Rio_conditions dataset
#'
#' This contains the weather conditions for the Personal Weather Stations in
#' Rio de Janeiro, Brazil, downloaded on March 12, 2016.
#' It is the output of the following call of the 'PWS_conditions' function,
#' Rio_metadata <- PWS_meta_query(-22.856878, -43.185368, 50, your.key)
#' where, your.key' should be replaced with your API-key for Weather Underground
#' and 'Rio_metadata' is provided in the package (load it via data(Rio_metadata)).
#'
#' @examples
#' data(Rio_conditions)
#' head(Rio_conditions)
#'
#' @author wundr team
"Rio_conditions"


#' Rio_history dataset
#'
#' This contains the weather history for the Personal Weather Stations in
#' Rio de Janeiro, Brazil, in the period Dec 24,2015 - Dec, 31 2015.
#' It is the output of the following call of the 'PWS_history' function,
#' Rio_history <- PWS_history(Rio_metadata,"20151224","20151231",your.key)
#' where, your.key' should be replaced with your API-key for Weather Underground
#' and 'Rio_metadata' is provided in the package (load it via data(Rio_metadata)).
#'
#' @examples
#' data(Rio_history)
#' head(Rio_history)
#'
#' @author wundr team
"Rio_history"









