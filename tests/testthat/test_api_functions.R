##
## Begin zohren code
##

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +                 Test script for functions in 'api_functions.R' of wundr package                 +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(wundr)

context("API-functions")

# Data to be used
data(Rio_metadata)


test_that("createCentroidTable",{
  #
  # Note: there are no check and error messages on the arguments of createCentroidTable
  # since the funciton is not supposed to be called directly by the user, but rather it
  # is only called internally by PWS_meta_query, which does those tests
  #
  # Check that the function indeed runs and that the output has the right dimensions
  expect_equal(ncol(createCentroidTable(-122,37,400,40)),2)
  # Check that the output is of correct type
  expect_equal(class(createCentroidTable(-122,37,400,40)),"matrix")
})



test_that("PWS_meta_query",{
  #
  # Note: Due to potential unliability of the API service (which we occasionally experienced)
  # as well as the missing user API key, we do not make any actual API calls in this
  # this testing suit, but rather test for internal consistency
  #
  # Check that the function procuces correct errors if arguments are worngly specified
  expect_error(PWS_meta_query(-43.185368,-22.856878, -50, "your.key"), "Radius must be positive.")
  expect_error(PWS_meta_query(-43.185368,-22.856878, 50, 129821221), "User key must be of type character.")
  expect_error(PWS_meta_query(300,-22.856878, 50, "your.key")) #
  expect_error(PWS_meta_query(-43.185368,2000, 50, "your.key")) #
})


test_that("PWS_meta_subset",{
  #data(Rio_metadata)
  # Check that the function runs correctly and that the output is of correct type
  expect_equal(typeof(PWS_meta_subset(Rio_metadata,-43.185368,-22.856878, 10)),"list")
  # Check that the result is of correct dimensions and specifications
  expect_equal(length(PWS_meta_subset(Rio_metadata,-43.185368,-22.856878, 10)),2)
  expect_equal(names(PWS_meta_subset(Rio_metadata,-43.185368,-22.856878, 10))[1],"PWSmetadata")
  expect_equal(names(PWS_meta_subset(Rio_metadata,-43.185368,-22.856878, 10))[2],"call")
  # Check that the function procuces correct errors if arguments are worngly specified
  expect_error(PWS_meta_subset(Rio_metadata,300,-22.856878, 10)) #
  expect_error(PWS_meta_subset(Rio_metadata,-43.185368,2000, 10)) #
  expect_error(PWS_meta_subset(Rio_metadata,-43.185368,-22.856878, -10), "Radius must be positive.")
  expect_error(PWS_meta_subset(Rio_metadata$PWSmetadata,-43.185368,-22.856878, 10), "Provide a valid meta data object.")
})




test_that("PWS_history",{
  #
  # Note: Due to potential unliability of the API service (which we occasionally experienced)
  # as well as the missing user API key, we do not make any actual API calls in this
  # this testing suit, but rather test for internal consistency
  #
  # Check that the function procuces correct errors if arguments are worngly specified
  expect_error(PWS_conditions(Rio_metadata, 129821221), "User key must be of type character.")
  expect_error(PWS_conditions(Rio_metadata$PWSmetadata, "your.key"), "Provide a valid meta data object.")
})




test_that("PWS_conditions",{
  #
  # Note: Due to potential unliability of the API service (which we occasionally experienced)
  # as well as the missing user API key, we do not make any actual API calls in this
  # this testing suit, but rather test for internal consistency
  #
  # Check that the function procuces correct errors if arguments are worngly specified
  expect_error(PWS_history(Rio_metadata,"20151224","20151231",32823239), "User key must be of type character.")
  expect_error(PWS_history(Rio_metadata,"20151224","20151241", "your.key"), "Dates must be in format 'YYYYMMDD' and in cronological order.")
  expect_error(PWS_history(Rio_metadata,"2015/12/24","2015/12/31", "your.key"), "Dates must be in format 'YYYYMMDD' and in cronological order.")
  expect_error(PWS_history(Rio_metadata,"20151231","20151224", "your.key"), "Dates must be in format 'YYYYMMDD' and in cronological order.")
})




##
## End zohren code
##
