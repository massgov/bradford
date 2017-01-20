library(testthat)
source("../functions/read_data.R")

context("read data functions within dashboard/functions/read_data.R")

# create some fake data so we do not have to commit RDS files
saveRDS("data/global.summary.RDS", list(c(1,2), c(3,4)))


test_that("readIntoList errors on incorrect input", {
  expect_error(readIntoList(data.dir = "../",
                            pattern = "^global.summary",
                            gsub.pattern = ".RDS"))
  expect_error(readIntoList(data.dir = "data/",
                            pattern = "onions",
                            gsub.pattern = ".RDS"))
})

test_that("readIntoList returns correct output", {
  expect_length(readIntoList(data.dir = "data/",
                             pattern = "^global.summary",
                             gsub.pattern = ".RDS"), n = 2)
  expect_type(readIntoList(data.dir = "data/",
                           pattern = "^global.summary",
                           gsub.pattern = ".RDS"), type = "list")
})
