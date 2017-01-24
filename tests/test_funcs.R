# unit tests for bradford repo functions
library(testthat)

source("../src/functions/detect_breakout_funcs.R")
source("../src/functions/binom_bayes_funcs.R")


context("createTimeBucket")
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

context("Bayesian Functions")
test_that("betaPosterior outputs the correct vector", {
  x <- readRDS(file = "test_data/test_post_df.RDS")
  expect_equal_to_reference(betaPosterior(x, prior.mean = .5, prior.n = 10, sample.n = "sample.n", affirm.n = "affirm.n"),
                            "test_data/test_posterior.RDS")
})

test_that("betaPosteriorMean outputs the correct scalar", {
  x <- readRDS(file = "test_data/test_post_df.RDS")
  expect_equal_to_reference(betaPosteriorMean(x, prior.mean = .5, prior.n = 10, sample.n = "sample.n", affirm.n = "affirm.n"),
                            "test_data/test_posterior_mean.RDS")
})
