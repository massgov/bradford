library(testthat)
source("../functions/helper.R")

context("helper functions within dashboard/functions/helper.R")

test_that("factorPercentage fails on incorrect input", {
  factor.vec <- factor(c("cats", "cats", "dogs", "rabbits"))
  expect_error(factorPercentage(factor.vec = list()))
  expect_error(factorPercentage(factor.vec = factor.vec, factor.value = "turtles"))
  expect_error(factorPercentage(factor.vec = matrix(), factor.value = "onions"))
})

test_that("factorPercentage outputs the correct answer", {
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

test_that("prettyPercent outputs the correct answer", {
  expect_equal(prettyPercent(num = 10, round.n = 0, is.percent.points = T), "10 %")
  expect_equal(prettyPercent(num = .576, round.n = 0, is.percent.points = F), "58 %")
  expect_equal(prettyPercent(num = .576, round.n = 1, is.percent.points = F), "57.6 %")
})

test_that("meanCount errors on incorrect input", {
  grouped.df <- tibble::tibble(x = c("cats", "cats", "dogs", "rabbits")) %>%
    dplyr::group_by(x)
  expect_error(meanCount(grouped.df = data.frame(), round.n = 0))
  expect_error(meanCount(grouped.df = tibble::tibble(), round.n = 0))
  expect_error(meanCount(grouped.df = matrix(), round.n = 0))
  expect_error(meanCount(grouped.df = list(), round.n = 0))
  expect_error(meanCount(grouped.df = grouped.df, round.n = -1))
  expect_error(meanCount(grouped.df = grouped.df, round.n = "1"))
})

test_that("meanCount outputs the correct answer", {
  grouped.df <- tibble::tibble(x = c("cats", "cats", "dogs", "rabbits")) %>%
    dplyr::group_by(x)
  expect_equal(meanCount(grouped.df = grouped.df, round.n = 0), 1)
  expect_equal(meanCount(grouped.df = grouped.df, round.n = 1), 1.3)
})

test_that("flagIncompleteTimeperiod errors on incorrect input", {
  ts.vector <- ts(lubridate::ymd("2017-01-01", "2017-01-02"))
  zoo.vector <- zoo::as.zoo(lubridate::ymd("2017-01-01", "2017-01-02"))
  flag.vector <- seq.Date(from = date(lubridate::now()), to = (date(lubridate::now()) + 3), by = "day")
  expect_error(flagIncompleteTimeperiod(reference.vector = ts.vector, time.unit = "day"))
  expect_error(flagIncompleteTimeperiod(reference.vector = zoo.vector, time.unit = "day"))
  expect_error(flagIncompleteTimeperiod(reference.vector = flag.vector, time.unit = "hour"))
  expect_error(flagIncompleteTimeperiod(reference.vector = flag.vector, time.unit = "year"))
})

test_that("flagIncompleteTimeperiod outputs the correct answer", {
  flag.vector <- seq.Date(from = date(lubridate::now()), to = (date(lubridate::now()) + 3), by = "day")
  expect_true(any(flagIncompleteTimeperiod(reference.vector = flag.vector, time.unit = "week")))
  expect_true(any(flagIncompleteTimeperiod(reference.vector = flag.vector, time.unit = "day")))
})