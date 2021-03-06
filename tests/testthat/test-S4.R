##
## Begin jamarin (John A. Marin) code
##

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +             Test script for functions in 'S4_Class_Initialization_Functions' of wundr package               +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(wundr)
context("S4.Classes")

# Data to be used for S4.Locations
data("PWS.Loc.Chicago")
#Data to be used for S4.Conditions:
data("PWS.Conds.Chicago")
# Data to be used for S4.Query.Subset:
data("PWS.Loc.Sub.Chicago")
# Data to be used for S4.History:
data("PWS.Hist.Chicago")

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +                          TESTS FOR S4 SLOT CAPTURE AND FORMATTING                                  +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


test_that("PWS.Conditions",{
  #data(PWS.Conds.Chicago)
  # Check that number of wundr S4 observations matches its sp S4 analogue
  expect_equal( nrow(PWS.Conds.Chicago@data), nrow(PWS.Conds.Chicago@spatialPtDF@data) )
  # Check that user is being provided a data.frame in the @data slot
  expect_identical( class(PWS.Conds.Chicago@data), class(PWS.Conds.Chicago@spatialPtDF@data) )
  # Verify that sp Spatial bounding box contains user selected longitude coordinates
  expect_true(PWS.Conds.Chicago@call[1] > PWS.Conds.Chicago@spatialPt@bbox[1,][1])
  # Verify that sp Spatial bounding box contains user selected longitude coordinates
  expect_true(PWS.Conds.Chicago@call[1] < PWS.Conds.Chicago@spatialPt@bbox[1,][2])
  # Verify that sp Spatial bounding box contains user selected latitude coordinates
  expect_true(PWS.Conds.Chicago@call[2] > PWS.Conds.Chicago@spatialPt@bbox[2,][1])
  # Verify that sp Spatial bounding box contains user selected latitude coordinates
  expect_true(PWS.Conds.Chicago@call[2] < PWS.Conds.Chicago@spatialPt@bbox[2,][2])
  # Verify that each column element in data.frame is a character (not a sublist)
  expect_is(class(PWS.Conds.Chicago@data[,"temp_c"]), "character")
  # verify that extraneous columns do not exist
  expect_error(PWS.Conds.Chicago@data[,"dewpoint"] ,"undefined columns selected")
  # # Check that you get the correct error when the time series is empty
  # expect_error(history_zoo(Rio_history,"WrongId","tempm"),"Time series is empty.")
})

test_that("PWS.Locations",{
  #data(PWS.Loc.Chicago)
  # Check that number of wundr S4 observations matches its sp S4 analogue
  expect_equal( nrow(PWS.Loc.Chicago@spatialPt@coords), nrow(PWS.Loc.Chicago@spatialPtDF@data) )
  # Verify that sp Spatial bounding box contains user selected longitude coordinates
  expect_true(PWS.Loc.Chicago@call[1] > PWS.Loc.Chicago@spatialPt@bbox[1,][1])
  # Verify that sp Spatial bounding box contains user selected longitude coordinates
  expect_true(PWS.Loc.Chicago@call[1] < PWS.Loc.Chicago@spatialPt@bbox[1,][2])
  # Verify that sp Spatial bounding box contains user selected latitude coordinates
  expect_true(PWS.Loc.Chicago@call[2] > PWS.Loc.Chicago@spatialPt@bbox[2,][1])
  # Verify that sp Spatial bounding box contains user selected latitude coordinates
  expect_true(PWS.Loc.Chicago@call[2] < PWS.Loc.Chicago@spatialPt@bbox[2,][2])
  # Verify that each column element in data.frame is a character (not a sublist)
  expect_is(class(PWS.Loc.Chicago@spatialPtDF
                  @data[,"distance_mi"]), "character")
  # verify that extraneous columns do not exist
  expect_error(PWS.Loc.Chicago@spatialPtDF@data[,"temp_c"] ,"undefined columns selected")
  # # Check that you get the correct error when the time series is empty
  # expect_error(history_zoo(Rio_history,"WrongId","tempm"),"Time series is empty.")
})


test_that("PWS.Query.Subset",{
  data("PWS.Loc.Sub.Chicago"); data("PWS.Loc.Chicago")
  # Verify that the subregion's max longitude is less than the region's
  expect_true(PWS.Loc.Sub.Chicago@spatialPt@bbox[1,][2] < PWS.Loc.Chicago@spatialPt@bbox[1,][2])
  #Verify that the subregion's min longitude is greater than the region's
  expect_true(PWS.Loc.Sub.Chicago@spatialPt@bbox[1,][1] > PWS.Loc.Chicago@spatialPt@bbox[1,][1])
  #Verify that the subregion's max latitude is less than the region's
  expect_true(PWS.Loc.Sub.Chicago@spatialPt@bbox[2,][2]  < PWS.Loc.Chicago@spatialPt@bbox[2,][2]
)
  #Verify that the subregion's min latitude is greater than the region's
  expect_true(PWS.Loc.Sub.Chicago@spatialPt@bbox[2,][1] > PWS.Loc.Chicago@spatialPt@bbox[2,][1] )
  #Verify that subregion has fewer observations
  nrow(PWS.Loc.Sub.Chicago@spatialPtDF@coords) < nrow(PWS.Loc.Chicago@spatialPtDF@coords)
})

test_that("PWS.History",{
  data("PWS.Hist.Chicago")
  #Verify uniformity of history data
  expect_equal(dim(PWS.Hist.Chicago@variance.Humidity), dim(PWS.Hist.Chicago@average.Temp))
  #Verify that original PWS.Location is within Wunderground History - lat below or equal max lat
  expect_true(PWS.Hist.Chicago@call$lat <= max(PWS.Hist.Chicago@history$latitude))
  #Verify that original PWS.Location is within Wunderground History - lat above or equal min lat
  expect_true(PWS.Hist.Chicago@call$lat >= min(PWS.Hist.Chicago@history$latitude))
  #Verify that original PWS.Location is within Wunderground History - llon below or equal max lon
  expect_true(PWS.Hist.Chicago@call$lon <= max(PWS.Hist.Chicago@history$longitude))
  #Verify that original PWS.Location is within Wunderground History - lon above or equal min lon
  expect_true(PWS.Hist.Chicago@call$lon >= min(PWS.Hist.Chicago@history$longitude))
  #Verify that output year is equal to or before current date
  expect_true(
    as.numeric(substring(PWS.Hist.Chicago@history$pretty[1],27,30)) <= as.numeric(substring(lubridate:::now(),1,4))
  )
  #Verify that output year is equal to or after 1945 (earliest date held by wunderground.com)
  expect_true(
    as.numeric(substring(PWS.Hist.Chicago@history$pretty[1],27,30)) >= 1945
  )
})
##
## End jamarin code
##



