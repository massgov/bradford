library(testthat)
library(methods)
source("../functions/helper.R")

context("Helper functions")

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
  from <- as.Date(lubridate::now())
  to <- as.Date(lubridate::now() + lubridate::days(3))
  flag.vector <- seq.Date(from = from, to = to, by = "day")
  expect_error(flagIncompleteTimeperiod(reference.vector = ts.vector, time.unit = "day"))
  expect_error(flagIncompleteTimeperiod(reference.vector = zoo.vector, time.unit = "day"))
  expect_error(flagIncompleteTimeperiod(reference.vector = flag.vector, time.unit = "hour"))
  expect_error(flagIncompleteTimeperiod(reference.vector = flag.vector, time.unit = "year"))
})

test_that("flagIncompleteTimeperiod outputs the correct answer", {
  from <- as.Date(lubridate::now())
  to <- as.Date(lubridate::now() + lubridate::days(3))
  flag.vector <- seq.Date(from = from, to = to, by = "day")
  expect_true(any(flagIncompleteTimeperiod(reference.vector = flag.vector, time.unit = "week")))
  expect_true(any(flagIncompleteTimeperiod(reference.vector = flag.vector, time.unit = "day")))
})

test_that("groupAndOrder errors on incorrect input", {
  test.df <- data.frame(a = factor(c("A", "A", "B", "C")), 
                        b = c(1, 1, 1, 1), 
                        c = c("A", "A", "B", "C"))
  expect_error(groupAndOrder(df = test.df, group.col = "a", data.col = "c"))
  expect_error(groupAndOrder(df = test.df, group.col = "b", data.col = "b"))
  expect_error(groupAndOrder(df = test.df, group.col = "a", data.col = "b", top.pct = 2))
})

test_that("groupAndOrder outputs the correct answer", {
  test.df <- data.frame(a = factor(c("A", "A", "B", "C")), 
                        b = c(1, 1, 1, 1), 
                        c = c("A", "A", "B", "C"))
  expect_equal(groupAndOrder(df = test.df, group.col = "a", data.col = "b", top.pct = 1), 
               data.frame(group = factor(c("A", "B", "C")), 
                          total = c(.5, .25, .25), 
                          cumul = c(.50, .75, 1)))

  # Fix for releveling 
 # expect_equal(groupAndOrder(df = test.df, group.col = "c", data.col = "b", top.pct = .8), 
  #              data.frame(group = c("A", "B"), 
   #                        total = c(.5, .25), 
    #                       cumul = c(.50, .75)))
})

test_that("getTopOrBottomK accepts correct input", {
  test.df <- data.frame(a = factor(c("A", "A", "B", "C")), 
                        b = c(1, 1, 1, 1), 
                        c = c("A", "A", "B", "C"))
  expect_error(getTopOrBottomK(df = test.df, group.col = "a", data.col = "b", k = 'd', get.top = TRUE))
  expect_error(getTopOrBottomK(df = test.df, group.col = "a", data.col = "b", k = 4, get.top = 'test'))
  expect_error(getTopOrBottomK(df = test.df, group.col = "a", data.col = "c", k = 4, get.top = FALSE))
  expect_error(getTopOrBottomK(df = test.df, group.col = "b", data.col = "c", k = 4, get.top = FALSE))
})

test_that("getTopOrBottomK returns correct values", {
  test.df <- data.frame(a = factor(c("A", "A", "B", "C","D","C")), 
                        b = c(1, 1, 1, 1, 3, 4))
  expect_equal(getTopOrBottomK(df = test.df, group.col = "a", data.col = "b", k = 1, get.top = TRUE), 
               test.df[test.df$a == 'C',])
  expect_equal(getTopOrBottomK(df = test.df, group.col = "a", data.col = "b", k = 6, get.top = TRUE), 
               test.df)
  expect_equal(getTopOrBottomK(df = test.df, group.col = "a", data.col = "b", k = 2, get.top = FALSE), 
               test.df[test.df$a %in% c('B','A'),])
  expect_equal(getTopOrBottomK(df = test.df, group.col = "a", data.col = "b", k = 3, get.top = FALSE), 
               test.df[test.df$a %in% c('A','B','D'),])
})

