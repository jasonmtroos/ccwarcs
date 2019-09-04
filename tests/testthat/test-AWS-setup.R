test_that("test AWS credentials", {
  skip_on_cran()
  
  testthat::expect_message(test_AWS_credentials(), 
                           "(AWS credentials were found)|(Unable to locate AWS credentials)")
})
