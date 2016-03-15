# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#    Test script for functions in 'cartoDB_functions.R' of wundr package
#
#    This file contains Matthew Schumwinger's (mjs13) code.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


library(wundr)

context("cdb-functions")

# Data to be used
data(matt_cdb_table)

test_that("r2cdb",{
  #
  # Check that the function procuces correct errors if arguments are worngly specified
  expect_error(r2cdb("your.cdb.key", "your.cdb.account", PWS.Loc.Chicago), "PWS.Conditions must be of class PWS.Conditions.")
  expect_error(r2cdb("your.cdb.key", 123456, PWS.Conds.Chicago), "cdb_account must be of type character.")
  expect_error(r2cdb(123456, "your.cdb.account", PWS.Conds.Chicago), "user_key must be of type character.")
})
