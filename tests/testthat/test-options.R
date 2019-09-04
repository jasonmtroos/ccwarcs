test_that("options validated", {
  ccwarcs_options()
  testthat::expect_error(ccwarcs_options(cache = NA), "cache.*character")
  testthat::expect_error(ccwarcs_options(cache = NULL), "cache.*character")
  testthat::expect_error(ccwarcs_options(cache = 0), "cache.*character")
  sayNA <- function(msg, default, prompts, ...) {
    NA
  }
  options(askYesNo = sayNA)
  testthat::expect_error(ccwarcs_options(cache = "asdf"), "no.*cache.*folder")
  options(askYesNo = NULL)

  testthat::expect_error(ccwarcs_options(cdx_sleep = -1), "cdx_sleep.*greater.*zero")
  testthat::expect_error(ccwarcs_options(cdx_sleep = NA), "cdx_sleep.*numeric")
  testthat::expect_error(ccwarcs_options(cdx_sleep = NULL), "cdx_sleep.*numeric")
  testthat::expect_error(ccwarcs_options(cdx_sleep = "asdf"), "cdx_sleep.*numeric")

  testthat::expect_error(ccwarcs_options(page_size = 1.3), "page_size.*integer")
  testthat::expect_error(ccwarcs_options(page_size = -1.3), "page_size.*positive")
  testthat::expect_error(ccwarcs_options(page_size = NA), "page_size.*integer")
  testthat::expect_silent(ccwarcs_options(page_size = NULL))
})
