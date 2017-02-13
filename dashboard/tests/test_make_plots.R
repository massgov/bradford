library(testthat)
library(ggplot2)
source("../functions/make_plots.R")

context("Plotting helpers")

test_that("printGGplotly errors on incorrect input" ,{
  x <- runif(100)
  y <- runif(100)
  plt <- plot(x = x, y = y)
  expect_error(printGGplotly(plt))
})

context("Plotting functions")

test_that("makeBlankPlot takes no args and outputs a ggplot object", {
  plt = makeBlankPlot()
  expect_true(is.ggplot(plt))
})

test_that("plotting functions pass empty plots if given df with no rows", {
  blank.plt = makeBlankPlot()
  expect_equal(makeBreakoutPlot(df = data.frame()), blank.plt)
  expect_equal(makeVolumeAreaPlot(df = data.frame()), blank.plt)
  expect_equal(makeVolumeBarPlot(df = data.frame()), blank.plt)
  expect_equal(makeAffirmativeBarPlot(df = data.frame()), blank.plt)
  expect_equal(makeGroupedPareto(df = data.frame()), blank.plt)
  expect_equal(makeGroupedTimeseries(df = data.frame()), blank.plt)
  expect_equal(buildParetoChart(grouped.df = data.frame()), blank.plt)
})

