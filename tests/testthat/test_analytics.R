# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +             Test script for functions in 'analytics_functions.R' of wundr package               +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##
## Begin zohren code
##

library(wundr)

context("Analytics-functions")

# Data to be used in test for temporal analysis functions:
data(Rio_history)
# Data to be used in test for spatial analysis functions:
data(Rio_conditions)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +                          TESTS FOR TEMPORAL ANALYSIS FUNCTIONS                                  +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


test_that("history_zoo",{
  #data(Rio_history)
  # Check that the time series has the correct length
  expect_equal(length(history_zoo(Rio_history,"IRIODEJA53","tempm")) ,nrow(subset(Rio_history, id=="IRIODEJA53")))
  # Check that the time series has the correct number of columns (variables)
  expect_equal(ncol(history_zoo(Rio_history,"IRIODEJA53",c("hum","tempm"))) , 2)
  # Check that we get an error when selecting undefined columns (variables)
  expect_error(history_zoo(Rio_history,"IRIODEJA53","wrongVariable"),"undefined columns selected")
  expect_error(history_zoo(Rio_history,"WrongId","wrongVariable"),"undefined columns selected")
  # Check that you get the correct error when the time series is empty
  expect_error(history_zoo(Rio_history,"WrongId","tempm"),"Time series is empty.")
})


test_that("history_ts",{
  #data(Rio_history)
  # Check that the time series has the correct length
  expect_equal(length(history_ts(Rio_history,"IRIODEJA53",c("tempm"))) ,nrow(subset(Rio_history, id=="IRIODEJA53")))
  # Check that the time series has the correct number of columns  (variables)
  expect_equal(ncol(history_ts(Rio_history,"IRIODEJA53",c("hum","tempm"))) , 2)
  # Check that we get an error when selecting undefined columns  (variables)
  expect_error(history_ts(Rio_history,"IRIODEJA53","wrongVariable"),"undefined columns selected")
  expect_error(history_ts(Rio_history,"WrongId","wrongVariable"),"undefined columns selected")
  # Check that you get the correct error when the time series is empty
  expect_error(history_ts(Rio_history,"WrongId","tempm"),"Time series is empty.")
})


test_that("history_forecast",{
  #data(Rio_history)
  # Check that we get a meaningfull fit with correct dimension
  expect_equal(length(history_forecast(history_ts(Rio_history,"IRIODEJA53","hum"))$fitted) ,length(history_ts(Rio_history,"IRIODEJA53","hum")))
  # Check that we get an error if the argument is not a time series
  expect_error(history_forecast(Rio_history) ,"Argument must be a time series of class 'zoo' or 'ts'.")
  # Check that we get an error if time series is not univariate
  expect_error(history_forecast(history_ts(Rio_history,"IRIODEJA53",c("hum","tempm"))),"Forecasting is only possible for univariate time series.")
})


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +                         TESTS FOR SPATIAL ANALYSIS FUNCTIONS                                    +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


test_that("create_geo_cond",{
  #data(Rio_conditions)
  # Check that output runs through and is of correct class
  expect_equal(class(create_geo_cond(Rio_conditions,"temp_c")), "geodata")
  # Check that the output has the right number of observations
  expect_equal(nrow(create_geo_cond(Rio_conditions,"temp_c")$coord),nrow(Rio_conditions))
  # Check that we get an error when selecting undefined columns (variables)
  expect_error(create_geo_cond(Rio_conditions,"wrongVariable"),"undefined columns selected")
  # Check that we get an error when variable is not of length 1
  expect_error(create_geo_cond(Rio_conditions,c("temp_c","temp_f")),"Select only one variable.")
})


test_that("create_grid",{
  #data(Rio_conditions)
  # Check that the function runs correctly and is of correct class
  expect_equal(class(create_grid(create_geo_cond(Rio_conditions,"temp_c"))), "matrix")
  # Check that the output has the right number of columns
  expect_equal(ncol(create_grid(create_geo_cond(Rio_conditions,"temp_c"))),2)
  # Check that we get an error when first argument is not of class 'geodata'
  expect_error(create_grid(Rio_conditions),"First argument must be of class 'geodata'.")
})



test_that("GP_fit",{
  #data(Rio_conditions)
  # Check that the function runs correctly and is of correct class
  expect_equal(class(GP_fit(create_geo_cond(Rio_conditions,"temp_c"))), "data.frame")
  # Check that we get an error when first argument is not of class 'geodata'
  expect_error(GP_fit(Rio_conditions),"First argument must be of class 'geodata'.")
})


##
## End zohren code
##

