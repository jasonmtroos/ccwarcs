test_that("test AWS credentials", {
  t <- purrr::quietly(test_AWS_credentials)
  o <- t()
  testthat::expect_true(
    any(stringr::str_detect(o$messages, "(AWS credentials were found)|(Unable to locate AWS credentials)"))
  )
})
