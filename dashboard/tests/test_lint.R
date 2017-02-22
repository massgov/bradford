library(testthat)
library(lintr)

if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Code is Lint Free", {
    x <- list.files(path = "../../", pattern = ".R$",
                    full.names = T, recursive = T) %>%
      purrr::map_df(function(x) lintr::lint(x) %>% data.frame()) %>%
      dplyr::select(filename, line_number, message)
    print(x)
    if (nrow(x) > 0) {
      print(c("Please lint and fix the following files:", unique(x$filename)))
    }
    expect_equal(nrow(x), 0)
  })
}
