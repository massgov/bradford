library(testthat)

context("read data functions within dashboard/functions/read_data.R")

test_that("readIntoList errors on incorrect input", {
  expect_error(readIntoList(data.dir = "~/Documents/GitHub/bradford/dashboard/tests/", 
                            pattern = "^global.summary", 
                            gsub.pattern = ".RDS"))
  expect_error(readIntoList(data.dir = "~/Documents/GitHub/bradford/dashboard/tests/data/",
                            pattern = "onions",
                            gsub.pattern = ".RDS"))
})

test_that("readIntoList returns correct output", {
  expect_length(readIntoList(data.dir = "~/Documents/GitHub/bradford/dashboard/tests/data/", 
                             pattern = "^global.summary", 
                             gsub.pattern = ".RDS"), n = 2)
  expect_type(readIntoList(data.dir = "~/Documents/GitHub/bradford/dashboard/tests/data/", 
                           pattern = "^global.summary", 
                           gsub.pattern = ".RDS"), type = "list")
})