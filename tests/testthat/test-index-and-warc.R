test_that("index and warcs works", {
  opts <- ccwarcs_options(cache = system.file("cache", package = "ccwarcs"))

  crawls <- cdx_fetch_list_of_crawls()$id
  expect_is(crawls, "character")
  expect_gte(length(crawls), 64)
  crawls <- crawls[seq_len(3)]
  urls <- c("r-project.org", "rstudio.com")
  cc_index <-
    get_cc_index(urls, crawls, .options = opts) %>%
    dplyr::filter(status == "200") %>%
    dplyr::group_by(urlkey) %>%
    dplyr::filter(timestamp == max((timestamp))) %>%
    dplyr::ungroup()
  expect_equal(nrow(cc_index), 2)

  warcs <-
    cc_index %>%
    dplyr::mutate(warc = get_warc(
      filename, offset, length, digest, include_headers = TRUE, 
      .options = opts)) %>%
    magrittr::extract2("warc")
  expect_equal(length(warcs), 2)
  expect_equal(length(warcs[[1]]), 2)
  expect_equal(length(warcs[[2]]), 2)
  expect_true(stringr::str_detect(
    warcs[[1]][[1]], "sha1:ILB6S7TS5WMLJVJIUBRQA53XRK2I3DN7"))
  expect_true(stringr::str_detect(
    warcs[[2]][[1]], "sha1:URJQ6YH4LS4I6XBFXYUPTN2MIHFBC7V4"))

  skip_if_not_installed("rvest")
  expect_true(
    rvest::minimal_html(warcs[[1]][[2]]) %>%
      rvest::html_text() %>%
      stringr::str_detect(
        stringr::str_c("R is a free software environment for ",
        "statistical computing and graphics"))
  )
  expect_true(
    rvest::minimal_html(warcs[[2]][[2]]) %>%
      rvest::html_text() %>%
      stringr::str_detect(
        "An integrated development environment for R")
  )
  
  expect_match(ccwarcs:::cdx_append_page_size_argument(
    'call',  page_size = 1234),  "&pageSize=1234")
  expect_true(ccwarcs:::ensure_directory_exists(normalizePath('~/')))
  
  skip_if_offline()
  skip_if_not(test_AWS_credentials())
  opts <- ccwarcs_options(cache = tempdir()) 
  idx <- get_cc_index("r-project.org", "2019-35", .options = opts)
  warc <- 
    idx %>%
    dplyr::filter(status == 200)  %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::mutate(page = get_warc(filename, offset, length, digest, 
                                  .options = opts))
  expect_true(
    warc$page %>%
      rvest::minimal_html() %>%
      rvest::html_text() %>%
      stringr::str_detect(
        stringr::str_c("R is a free software environment for ",
        "statistical computing and graphics"))
  )
  
  warc <- 
    idx %>%
    dplyr::filter(status == 200)  %>%
    dplyr::filter(dplyr::row_number() == 1)
  warc <- get_warc(warc$filename, warc$offset, warc$length, 
                   warc$digest, include_headers = TRUE, 
                   .options = opts)
  expect_type(warc, 'list')
  expect_length(warc, 2)
  
  
  warcs <-
    cc_index %>%
    dplyr::filter(status == 200) %>%
    dplyr::mutate(warc = get_warc(
      filename, offset, length, digest, include_headers = FALSE, 
      .options = opts))
  expect_length(warcs$warc,  2)
  
})
