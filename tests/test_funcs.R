# unit tests for bradford repo functions
library(testthat)

source("~/Documents/GitHub/bradford/src/funcs.R")

test_that("createTimeBucket Errors out on incorrect input", {
  expect_error(createTimeBucket(x = "abc"))
  expect_error(createTimeBucket(x = factor("abc")))
  expect_error(createTimeBucket(x = list(1,2,3)))
})

test_that("createTimeBucket throws a warning when values fall beyond 0-23", {
  expect_warning(createTimeBucket(x = c(1,2,NA)))
  expect_warning(createTimeBucket(x = c(-1, 50)))
})

test_that("createTimeBucket returns NAs for values falling beyond 0-23", {
  expect_identical(createTimeBucket(x = c(1,2,-1,50)), factor(c("Early AM", "Early AM", NA, NA)))
})

