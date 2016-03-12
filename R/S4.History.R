require(lubridate)

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
            
            history="data.frame"
  )
)

PWS.History <- function(...) return(new(Class="PWS.History",...))

#PWSmetadata <- PWS.Locations@spatialPtDF@data # feeds into lower level function as 

as.Date("20160311", "%Y%m%d") < as.Date("19450101", "%Y%m%d")

as.Date("20160311", "%Y%m%d") 
typeof(2)

setMethod("initialize",
          "PWS.History",
          
          function(.Object, PWS.Locations, begin_YYYYMMDD, end_YYYYMMDD, user.key,...){
            
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
            
            measure <- readline(prompt="Metric or Imperial Units: (m,i)?  \n \n")
            
            PWSmetadata <- PWS.Locations@spatialPtDF@data
            
            history <- S4.history(PWSmetadata, begin_YYYYMMDD, end_YYYYMMDD, user.key)
            
            if (substring(tolower(measure),1,1) == "i" ){
              
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
              
              .Object@history <- history[,-c(11,22,13)]
            }
            
            if (substring(tolower(measure),1,1) == "m" ){
              
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
              
              .Object@history <- history[,-c(12,23,14)]
            }
            return(.Object)
          }
)

##
##  EXAMPLE
##
# h.S4 <- PWS.History(a, "20160306", "20160306", user.key)
# View(h.S4@history)

S4.history <- function(PWSmetadata,begin_YYYYMMDD,end_YYYYMMDD,user_key , 
                       stdAPI = TRUE){
  #  
  # Constants:
  url_base <- "http://api.wunderground.com/api/"
  
  # Error tests on inputs:
  if(typeof(user_key)!="character") stop("User key must be of type character.")
  
  history <- NULL
  count = 0
  date_list <- tryCatch(seq(as.Date(begin_YYYYMMDD,"%Y%m%d"), 
                            as.Date(end_YYYYMMDD,"%Y%m%d"), by="days"),
                        error = function(e) {stop("Dates must be in format 'YYYYMMDD' and in chronological order.")})
  
  date_list = gsub("-","",date_list)
  cat("A total of ",nrow(PWSmetadata)*length(date_list),
      " API calls is needed to download the metadata.\n")
  if(stdAPI) cat("Under standard API settings only 10 calls per minute are allowed.\n")
  
  cat("Downloading ")
  
  for(date in date_list){
    for(i in 1:nrow(PWSmetadata)){
      if(stdAPI & ( count%% 10 ==0 ) & count !=0) {
        cat(" Pausing ")
        Sys.sleep(60)
      }
      
      tmp.list <- fromJSON(paste0(url_base,user_key,"/history_",date,"/q/pws:",
                                  PWSmetadata$id[i] ,".json"))
      # Check JSON for error, i.e. wrong key etc.
      if(!is.null(tmp.list$response$error)) stop(paste("JSON error:",tmp.list$response$error$description))
      #
      tmp.data <- tmp.list$history$observations$date   
      if(!is.null(tmp.data)) {
        tmp.data$id <- rep(PWSmetadata$id[i],nrow(tmp.data))
        tmp.data$latitude <- rep(PWSmetadata$lat[i],nrow(tmp.data))
        tmp.data$longitude <- rep(PWSmetadata$lon[i],nrow(tmp.data))
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


