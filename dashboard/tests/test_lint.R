library(testthat)
library(lintr)

if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Code is Lint Free", {
    x <- list.files(path = "../../", pattern = ".R$", full.names = T, recursive = T) %>%
      purrr::map_df(function(x) lintr::lint(x) %>% data.frame())
    print(x)
    expect_equal(nrow(x), 0)
  })
}
