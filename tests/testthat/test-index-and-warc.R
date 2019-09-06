test_that("index works", {
  opts <- ccwarcs_options(cache = system.file('cache', package = 'ccwarcs'))
  
  crawls <- cdx_fetch_list_of_crawls()$id
  expect_is(crawls, 'character')
  expect_gte(length(crawls), 64)
  crawls <- crawls[seq_len(3)]
  urls <- c('r-project.org', 'rstudio.com')
  cc_index <- 
    get_cc_index(urls, crawls, .options = opts)  %>%
    dplyr::filter(status == "200") %>%
    dplyr::group_by(urlkey) %>%
    dplyr::filter(timestamp == max((timestamp))) %>%
    dplyr::ungroup()
  expect_equal(nrow(cc_index), 2)
  
  warcs <- 
    cc_index %>%
    dplyr::mutate(warc = get_warc(filename, offset, length, digest, include_headers = TRUE, .options  = opts)) %>%
    magrittr::extract2('warc')
  expect_equal(length(warcs), 2)
  expect_equal(length(warcs[[1]]), 2)
  expect_equal(length(warcs[[2]]), 2)
  expect_true(stringr::str_detect(warcs[[1]][[1]], "sha1:ILB6S7TS5WMLJVJIUBRQA53XRK2I3DN7"))
  expect_true(stringr::str_detect(warcs[[2]][[1]], "sha1:URJQ6YH4LS4I6XBFXYUPTN2MIHFBC7V4"))
  
  skip_if_not_installed('rvest')
  expect_true(
    rvest::minimal_html(warcs[[1]][[2]]) %>%
    rvest::html_text() %>%
    stringr::str_detect("R is a free software environment for statistical computing and graphics")
  )
  expect_true(
    rvest::minimal_html(warcs[[2]][[2]]) %>%
    rvest::html_text() %>%
    stringr::str_detect("An integrated development environment for R")
  )
})
