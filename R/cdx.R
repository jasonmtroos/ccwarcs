
cached_index_path <- function(x, .options) {
  cache <- .options$cache
  root <- "index"
  dir <- stringr::str_sub(x, 1, 2)
  fn <- x
  ext <- ".rds"
  stringr::str_c(path.expand(file.path(cache, root, dir, fn)), ext)
}
cdx_append_page_size_argument <- function(api_call, page_size = NULL) {
  if (!is.null(page_size)) {
    api_call <- stringr::str_c(api_call, "&pageSize=", page_size)
  }
  api_call
}
cdx_get_number_of_pages_for_index <- function(api_url, .options) {
  Sys.sleep(.options$cdx_sleep)

  api_call <- cdx_append_page_size_argument(stringr::str_c(api_url, "&showNumPages=TRUE"), .options$page_size)
  page_info <- jsonlite::fromJSON(api_call)
  page_info$pages
}
cdx_fetch_index_page <- function(api_url, page, .options) {
  Sys.sleep(.options$cdx_sleep)

  api_call <- cdx_append_page_size_argument(stringr::str_c(api_url, "&output=json&page=", page), .options$page_size)
  response <- httr::GET(api_call)
  results <- httr::content(response)
  json_results <- stringr::str_c("[", stringr::str_replace_all(stringr::str_trim(results, side = "right"), "\n", ", "), "]")
  jsonlite::fromJSON(json_results)
}
cdx_fetch_index <- function(api_url, .options) {
  opts <- furrr::future_options(globals = FALSE, scheduling = TRUE)
  num_pages <- cdx_get_number_of_pages_for_index(api_url, .options)
  furrr::future_map_dfr(-1 + seq_len(num_pages),
    ~ cdx_fetch_index_page(api_url, page = .x, .options = .options),
    .progress = TRUE, .options = opts
  )
}


#' Get a List of Indexes from the Common Crawl Index Server
#'
#' @return A tibble
#' @export
#'
#' @examples
#' # not run:
#' # cdx_fetch_list_of_indexes()
cdx_fetch_list_of_indexes <- function() {
  indexes <- tibble::as_tibble(jsonlite::fromJSON("https://index.commoncrawl.org/collinfo.json"))
  indexes %>%
    dplyr::transmute(id = stringr::str_replace(.data$id, "CC-MAIN-", ""), .data$name)
}


#' Get a List of WARCs from Common Crawl Index Server
#'
#' The list of WARCs is cached in a directory specified by the ```cache```  argument to [ccwarcs_options]
#'
#' @param urls A vector of URLs of captured pages, allowing `*` as a wildcard character
#' @param indexes A vector of Ids of CC indexes to search
#'
#' `index` is typically in the format `YYYY-ww`, e.g. 2018-47 for the crawl published in the 47th week of 2018.
#' See <https://index.commoncrawl.org/> for a list of crawls, and [cdx_fetch_list_of_indexes] for programmatic access
#' to this list.
#'
#' @param .options An optional object of class [ccwarcs_options]
#' @return A tibble
#' @export
#'
#' @examples
#' # not run:
#' # url <- "http://www.celebuzz.com/2017-01-04"
#' # index <- "2018-47"
#' # results <- get_cc_index(url, index)
get_cc_index <- function(urls, indexes, .options = NULL) {
  if (is.null(.options)) {
    .options <- ccwarcs_options()
  }
  uil <- as.list(tidyr::crossing(urls, indexes))
  purrr::map2_dfr(
    .x = as.list(uil$urls), .y = as.list(uil$indexes),
    ~ get_cc_index_impl(url = .x, index = .y, .options = .options)
  )
}
get_cc_index_impl <- function(url, index, .options) {
  api_call <- stringr::str_glue("http://index.commoncrawl.org/CC-MAIN-{index}-index?url={url}")
  index_key_digest <- digest(stringi::stri_enc_toutf8(api_call))
  index_cache_results_fn <- cached_index_path(index_key_digest, .options)
  if (file.exists(index_cache_results_fn)) {
    result <- tibble::as_tibble(readRDS(file = index_cache_results_fn))
  } else {
    ensure_directory_exists(index_cache_results_fn)
    index_results <- cdx_fetch_index(api_call, .options = .options)
    saveRDS(object = index_results, file = index_cache_results_fn)
    result <- tibble::as_tibble(index_results)
  }
  dplyr::mutate_at(result, dplyr::vars(dplyr::matches("offset|status|length")), list(as.integer))
}
