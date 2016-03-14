#' @include S4.Locations.R S4.Conditions.R S4.Query.Subset.R

setMethod("plot", signature(x=c("PWS.Locations")),
          function(x,y,...) {
            webmap_pnts(x)
          })

setMethod("plot", signature(x=c("PWS.Conditions")),
          function(x,y,...) {
            webmap_pnts(x)
          })

setMethod("plot", signature(x=c("PWS.Query.Subset")),
          function(x,y,...) {
            webmap_pnts(x)
          })

