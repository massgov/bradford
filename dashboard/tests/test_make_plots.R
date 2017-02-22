library(testthat)
source("../functions/make_plots.R")

context("Plotting helpers")

test_that("printGGplotly errors on incorrect input", {
  x <- runif(100)
  y <- runif(100)
  plt <- plot(x = x, y = y)
  expect_error(printGGplotly(plt))
               })
