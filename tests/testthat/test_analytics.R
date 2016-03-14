
library(wundr)
context("Analytics")

data(Rio_history)

test_that("history_zoo",{
  data(Rio_history)
  expect_equal(length(history_zoo(Rio_history,"IRIODEJA53","tempm")) ,nrow(subset(Rio_history, id=="IRIODEJA53")))
  expect_equal(ncol(history_zoo(Rio_history,"IRIODEJA53",c("hum","tempm"))) , 2)
  expect_error(history_zoo(Rio_history,"IRIODEJA53","wrongVariable"),"undefined columns selected")
})


test_that("history_ts",{
  data(Rio_history)
  expect_equal(length(history_ts(Rio_history,"IRIODEJA53",c("tempm"))) ,nrow(subset(Rio_history, id=="IRIODEJA53")))
  expect_equal(ncol(history_ts(Rio_history,"IRIODEJA53",c("hum","tempm"))) , 2)
  expect_error(history_ts(Rio_history,"IRIODEJA53","wrongVariable"),"undefined columns selected")
})
