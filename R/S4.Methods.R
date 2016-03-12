
setMethod("plot", signature(x=c("PWS.locations")),
          function(x,y,...) {
            webmap_pnts(x)
          })

setMethod("plot", signature(x=c("PWS.conditions")),
          function(x,y,...) {
            webmap_pnts(x)
          })

setMethod("plot", signature(x=c("PWS.Query.Subset")),
          function(x,y,...) {
            webmap_pnts(x)
          })
