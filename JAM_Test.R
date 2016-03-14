jam.key <- "c15be91d68fbf265"
jam.key.2 <- "00d9766eedab434e"

saveRDS(ch_cond, "ch_cond.rds")
readRDS("ch_cond.rds")


PWS.Loc.Chicago <- PWS.Locations("Chicago, IL", radius=5, user.key=jam.key.2)
PWS.Conds.Chicago <- PWS.Conditions(PWS.Loc.Chicago, user.key=jam.key.2)
PWS.Hist.Chicago <- PWS.History(PWS.Loc.Chicago, "20150306", "20150310", user.key=jam.key.2)
saveRDS(PWS.Loc.Chicago, "data/PWS.Loc.Chicago.rds")
saveRDS(PWS.Conds.Chicago, "data/PWS.Conds.Chicago.rds")
saveRDS(PWS.Hist.Chicago , "data/PWS.Hist.Chicago.rds")



nrow(PWS.Loc.Chicago@spatialPtDF@data)
PWS.Conds.Chicago@data
PWS.Hist.Chicago@average.Temp

setGeneric("density", function(object) standardGeneric("density"))

setMethod("density", signature(x=c("PWS.Locations")),
          function(object,...) {
            simple_density(object,...)
          })

setMethod("marray", "MArray",
          +           function(object) object@marray)


args(simple_density)

density(PWS.Loc.Chicago)
PWS.Loc.Chicago@spatialPtDF@data
PWS.Conds.Chicago@data

setValidity("PWS.Conditions",
            function(object){
              TRUE
              if(nrow(object@data) != nrow(object@spatialPtDF@data) ) stop("Row numbers don't match")
            })

validObject(PWS.Conds.Chicago)
