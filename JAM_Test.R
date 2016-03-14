jam.key <- "c15be91d68fbf265"
jam.key.2 <- "00d9766eedab434e"

#' @include S4.Locations.R S4.Conditions.R S4.Query.Subset.R

# PWS.Locations("Santa Monica, CA", radius=3, user.key=jam.key)
#
# PWS.L <- PWS.Locations(-118.49119, 34.01945, radius=3, user.key=jam.key)
# PWS.L
# PWS.L@spatialPtDF

#PWS.Locations("Santa Monica, CA", radius=3, user.key=user.key)

#PWS.Locations(-118.49119, 34.01945, radius=3, user.key=user.key)

##  EXAMPLE
##
# C.S4 <- PWS.Conditions(PWS.L, jam.key)
# C.S4.sub <- PWS.Conditions(PWS.Sub, jam.key)
# View(C.S4.sub@data)
# h.S4.sub
# plot(C.S4)
# View(C.S4@data)

##
##  EXAMPLE
##
##
# h.S4.sub <- PWS.History(PWS.Sub, "20160306", "20160306", jam.key)
# View(h.S4.sub@history)
# h.S4 <- PWS.History(PWS.L, "20160306", "20160306", jam.key)
#
# View(h.S4@history)

#PWS.Loc.Sub.Chicago <- PWS.Query.Subset(PWS.Loc.Chicago, -87.62, 41.88, 2)
# PWS.Loc.Sub.Chicago
##
## EXAMPLE SUBSETTING
## PWS.Sub <- PWS.Query.Subset(PWS.L, -118.49, 34.02, 2)
# PWS.Locations.Query@spatialPtDF@bbox
# a@call
# isS4(PWS.MetaQuery)
# a <- PWS.L

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


args(simple_density)

density(PWS.Loc.Chicago)
PWS.Loc.Chicago@spatialPtDF@data
PWS.Conds.Chicago@data

setValidity("PWS.Conditions",
            function(object){
              TRUE
              if(nrow(object@data) != nrow(object@spatialPtDF@data) ) {stop("Row numbers don't match")}
            })

validObject(PWS.Loc.Chicago)

args(gg_points)

setGeneric("plot.gg", function(object,...) standardGeneric("plot.gg"))

setMethod("plot.gg", signature(object = "PWS.Locations"),
          function(object, zoom=12) {
            basemap <- set_basemap(object, zoom)
            gg_points(object, basemap=basemap, ...)
          })

setMethod("plot.gg", signature(object = "PWS.Conditions"),
          function(object, zoom=12) {
            basemap <- set_basemap(object, zoom)
            gg_points(object, basemap=basemap, ...)
          })

setMethod("plot.gg", signature(object = "PWS.Query.Subset"),
          function(object, zoom=12) {
            basemap <- set_basemap(object, zoom)
            gg_points(object, basemap=basemap, ...)
          })



data(PWS.Conds.Chicago)

plot.gg(PWS.Conds.Chicago)
plot.gg(PWS.Loc.Chicago)

gg_points(PWS.Conds.Chicago)

setMethod("sides", signature(object = "Polygon"), function(object) {
  object@sides
})

setMethod("plot", signature(x=c("PWS.Query.Subset")),
          function(x,y,...) {
            webmap_pnts(x)
          })



matt.cdb.key <- "f09ad502b34fa4096a62ea306b4650337d41009c"
matt.cdb.account <- "biglakedata"
#data("PWS.Conds.Chicago")
ibiza <- PWS.Conds.Chicago
r2cdb(matt.cdb.key, matt.cdb.account, ibiza)

#seven day temperature forecast

> chic.zoo.temp = history_zoo(PWS.Hist.Chicago@history, 'KILCHICA130', 'tempi')
> plot(chic.zoo.temp, col='red', main="temp")
View(chic.zoo.temp)


forkast <- function(hist.object) {
  object <- hist.object
  ID.Vector = unique(object@history$id)

  sapply(ID.Vector, function(id) {
    chic.zoo.temp <- history_zoo(object@history, id , 'tempi')
    ts.chic  <-  history_ts( object@history, id, 'tempi' )
    history_forecast(ts.chic) } )
}

forkast(PWS.Hist.Chicago)

setGeneric("forecast.10day", function(object,...) standardGeneric("forecast.10day"))

setMethod("forecast.10day", signature(object = "PWS.History"),
          function(object) {
          if(class(object) != 'PWS.History') stop('Please use a PWS.History S4 class object only')
          forkast(object)
          })

Chicago.forecast.10day <- forecast.10day(PWS.Hist.Chicago)
devtools::use_data(Chicago.forecast.10day)

heatmap(as.matrix(as.data.frame(Chicago.forecast.10day[[1]])))

heatmap(Chicago.forecast.10day[1])
nrow(PWS.Loc.Chicago@spatialPtDF@data)
library(sp)

PWS.Loc.Sub.Chicago <- PWS.Query.Subset(PWS.Loc.Chicago, -87.62, 41.88, 2)

install.packages('RUnit')
require(RUnit)

row.check <- function(c) return( nrow(c) )

row.test <- function(object.location) {
  object <- object.location
  checkEquals( nrow(object@spatialPtDF@data$id), nrow(object@spatialPt@coords), "Checking Row Equality Across Objects" )
}

testsuite.wundr <- defineTestSuite(name = "row.test",
                                 dirs = file.path(path.package(package="wundr"), "tests"),
                                 testFileRegexp = "^runit.+\\.R",
                                 testFuncRegexp = "^test.+",
                                 rngKind = "Marsaglia-Multicarry",
                                 rngNormalKind = "Kinderman-Ramage")


runTestFile(file.path(system.file(package="wundr"), "tests/row.test.r"))

testResult <- runTestSuite(testsuite.wundr)



test.chicago.data(PWS.Loc.Chicago)


library(testthat)
expect_equal( length(PWS.Loc.Chicago@spatialPtDF@data$id) + 1, nrow(PWS.Loc.Chicago@spatialPt@coords) )

nrow(PWS.Loc.Chicago@spatialPtDF@data$id)
nrow(PWS.Loc.Chicago@spatialPt@coords)

library(stringr)
?context
library(testthat)
testthat::context("dd")


test_that("str_length is number of characters", {
  expect_equal(str_length("a"), 1)
  expect_equal(str_length("ab"), 2)
  expect_equal(str_length("abc"), 3)
})

PWS.Loc.Chicago@spatialPtDF@data$id

PWS.Loc.Chicago@spatialPt@coords



  setValidity("PWS.Conditions",
              function(object){
                TRUE
                if(nrow(object@data) != nrow(object@spatialPtDF@data) ) {stop("Row numbers don't match")}
              })

validObject(PWS.Loc.Chicago)
