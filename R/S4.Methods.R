#' @include S4.Locations.R
#' @include S4.Conditions.R
#' @include S4.Query.Subset.R
#'
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

