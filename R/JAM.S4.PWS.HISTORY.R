setClass(
  Class = "PWS.History",
  slots = c(average.Temp="data.frame",
            average.Humidity="data.frame", 
            variance.Temp="data.frame",
            variance.Humidity="data.frame",
            standard.Dev.Temp="data.frame",
            standard.Dev.Humidity="data.frame",
            history="data.frame"
            )
)

PWS.History <- function(...) return(new(Class="PWS.History",...))

#PWSmetadata <- PWS.Locations@spatialPtDF@data # feeds into lower level function as 

setMethod("initialize",
          "PWS.History",
          
          function(.Object, PWS.Locations, begin_YYYYMMDD, end_YYYYMMDD, user.key,...){
            
            PWSmetadata <- PWS.Locations@spatialPtDF@data
            
            history <- S4.history(PWSmetadata, begin_YYYYMMDD, end_YYYYMMDD, user.key)
            
              .Object@average.Temp <- aggregate(tempi ~ id, data=history, mean)
              .Object@average.Humidity <- aggregate(hum ~ id, data=history, mean)
              .Object@variance.Temp <- aggregate(tempi ~ id, data=history, var)
              .Object@variance.Humidity <- aggregate(hum ~ id, data=history, var)
              .Object@standard.Dev.Temp <- aggregate(tempi ~ id, data=history, sd)
              .Object@standard.Dev.Humidity <- aggregate(hum ~ id, data=history, sd)
              .Object@history <- history

            return(.Object)
          }
)

# h.S4 <- PWS.History(a, "20160305", "20160306", user.key)
# h.S4@variance.Humidity
# h.S4@average.Temp
# merge(h.S4@average.Temp, h.S4@variance.Humidity)
