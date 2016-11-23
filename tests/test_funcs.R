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

test_that("createTimeBucket returns the expected values given an input",{
  expect_equivalent(createTimeBucket(x = c(0,1,2,3,4,5)), factor(c(rep("Early AM", 6))))
  expect_equivalent(createTimeBucket(x = c(6,7,8,9,10,11)), factor(c(rep("AM", 6))))
  expect_equivalent(createTimeBucket(x = c(12,13,14,15,16)), factor(c(rep("Afternoon", 5))))
  expect_equivalent(createTimeBucket(x = c(17,18,19,20)), factor(c(rep("Evening", 4))))
  expect_equivalent(createTimeBucket(x = c(21, 22, 23)), factor(c(rep("Late Night", 3))))
})
