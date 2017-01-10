library(testthat)
source("~/Documents/GitHub/bradford/dashboard/functions/helper.R")

context("helper functions within dashboard/functions/helper.R")

test_that("factorPercentage fails on incorrect input", {
  factor.vec <- factor(c("cats", "cats", "dogs", "rabbits"))
  expect_error(factorPercentage(factor.vec = list()))
  expect_error(factorPercentage(factor.vec = factor.vec, factor.value = "turtles"))
  expect_error(factorPercentage(factor.vec = matrix(), factor.value = "onions"))
})

test_that("factorPercentage outputs the right answer", {
  factor.vec <- factor(c("cats", "cats", "dogs", "rabbits"))
  expect_equal(factorPercentage(factor.vec = factor.vec, factor.value = "cats"), 50)
})

test_that("prettyPercent fails on incorrect input", {
  expect_error(prettyPercent(num = "10", round.n = 0, is.percent.points = T))
  expect_error(prettyPercent(num = 10, round.n = "0", is.percent.points = T))
  expect_error(prettyPercent(num = 10, round.n = 0, is.percent.points = NA))
  expect_error(prettyPercent(num = 10, round.n = -2, is.percent.points = T))
})

test_that("prettyPercent warns on illogical input", {
  expect_warning(prettyPercent(num = 100000, round.n = 0, is.percent.points = T))
})

test_that("prettyPercent outputs the right answer", {
  expect_equal(prettyPercent(num = 10, round.n = 0, is.percent.points = T), "10 %")
  expect_equal(prettyPercent(num = .576, round.n = 0, is.percent.points = F), "58 %")
  expect_equal(prettyPercent(num = .576, round.n = 1, is.percent.points = F), "57.6 %")
})
