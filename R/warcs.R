#' Test AWS credentials
#'
#' A helper function that wraps [locate_credentials][aws.signature::locate_credentials] and provides
#' advice if AWS credentials cannot be found.
#'
#' @seealso [locate_credentials][aws.signature::locate_credentials]
#' @export
test_AWS_credentials <- function() {
  creds <- aws.signature::locate_credentials(verbose = TRUE)
  not_set <- is.null(creds$key)
  if (not_set) {
    h <- c(
      stringr::str_c("Unable to locate AWS credentials. ",
      "Consider taking one of the following two actions:"),
      stringr::str_wrap(stringr::str_c("1. Set the environment ",
      "variables ('AWS_ACCESS_KEY_ID', 'AWS_SECRET_ACCESS_KEY', ",
      "'AWS_DEFAULT_REGION', and 'AWS_SESSION_TOKEN')"), exdent = 3),
      stringr::str_wrap(stringr::str_c("2. Install and configure ",
      "the AWS Command Line Interface https://docs.aws.amazon.com/",
      "cli/latest/userguide/cli-chap-welcome.html"), exdent = 3)
    )
  } else {
    h <- "AWS credentials were found."
  }
  message(stringr::str_c(h, collapse = "
"))
  invisible(!not_set)
}

cached_warc_path <- function(x, .options) {
  cache <- .options$cache
  root <- "warcs"
  dir <- stringr::str_sub(x, 1, 2)
  fn <- x
  ext <- ".gz"
  stringr::str_c(path.expand(file.path(cache, root, dir, fn)), ext)
}

fetch_warc <- function(filename, start, end) {
  aws.s3::get_object(filename, "commoncrawl",
    headers = list("Range" = stringr::str_glue("bytes={start}-{end}"))
  )
}
cache_warc <- function(warc, warc_cache_file_fn) {
  ensure_directory_exists(warc_cache_file_fn)
  con <- file(warc_cache_file_fn, "wb")
  writeBin(warc, con)
  close(con)
}
read_warc_from_gzcon <- function(con, include_headers = FALSE) {
  if (class(con) == "raw") {
    con <- gzcon(rawConnection(con))
  } else if (class(con) == "character") {
    con <- gzcon(file(con, "rb"))
  }
  warc <- readr::read_lines(con)
  close(con)
  body_rows <- which(!stringr::str_detect(
    warc[seq_len(70)], "(^HTTP|^WARC|^[A-Za-z\\-\\_]+:|^$)"))
  if (length(body_rows) > 0) {
    warc_header_end <- min(body_rows, na.rm = TRUE) - 1L
  } else {
    warc_header_end <- length(warc)
  }
  payload <- stringr::str_c(warc[
    seq(warc_header_end + 1L, length(warc))], collapse = "\n")
  if (include_headers) {
    headers <- stringr::str_c(
      warc[seq_len(warc_header_end)], collapse = "\n")
    return(list(headers = headers, payload = payload))
  } else {
    return(payload)
  }
}
#' Get a WARC from the Common Crawl via AWS
#'
#' The WARC is cached in a directory specified by the 
#' ```cache```  argument to [ccwarcs_options]
#'
#' @param filename AWS path to the WARC
#' @param offset Starting byte offset for the chunk
#' @param length Number of bytes in the chunk
#' @param digest Common Crawl digest for requested chunk
#' @param include_headers If TRUE, include the WARC 
#'   and HTTP headers in the result. See below.
#' @param .options An optional object of class [ccwarcs_options]
#'
#' @return HTML contents of the requested WARC, and optionally the WARC and HTTP
#'   headers. If `include_headers = TRUE`, the result is a list with elements
#'   `headers` and `payload`. Otherwise the result is a character string
#'   (vector) containing the HTML of the requested WARC.
#' @examples
#' \dontrun{
#' warc <- get_warc(filename = paste0("crawl-data/CC-MAIN-2019-35/segments/1566027317516.88/",
#'            "warc/CC-MAIN-20190822215308-20190823001308-00434.warc.gz"),
#'          offset = 938342027, length = 3019, digest = "ILB6S7TS5WMLJVJIUBRQA53XRK2I3DN7", 
#'          include_headers = TRUE)
#' cat(warc[[1]])
#' cat(warc[[2]])
#' }
#' @export
get_warc <- function(filename, offset, length, digest, 
                     include_headers = FALSE, .options = NULL) {
  if (length(filename) > 1) {
    opts <- furrr::future_options(globals = FALSE, scheduling = TRUE)
    if (include_headers) {
      furrr::future_pmap(list(
        filename = as.list(filename), offset = as.list(offset),
        length = as.list(length), digest = as.list(digest)
      ),
      ~ get_warc_impl(..1, ..2, ..3, ..4, include_headers = TRUE, 
                      .options = .options),
      .progress = TRUE, .options = opts
      )
    } else {
      furrr::future_pmap_chr(list(
        filename = as.list(filename), offset = as.list(offset),
        length = as.list(length), digest = as.list(digest)
      ),
      ~ get_warc_impl(..1, ..2, ..3, ..4, include_headers = FALSE, 
                      .options = .options),
      .progress = TRUE, .options = opts
      )
    }
  } else {
    get_warc_impl(filename, offset, length, digest, 
                  include_headers, .options = .options)
  }
}
get_warc_impl <- function(filename, offset, length, digest, 
                          include_headers = FALSE, .options = NULL) {
  if (is.null(.options)) {
    .options <- ccwarcs_options()
  }
  warc_cache_file_fn <- cached_warc_path(digest, .options)
  if (file.exists(warc_cache_file_fn)) {
    warc <- read_warc_from_gzcon(warc_cache_file_fn, include_headers)
  } else {
    start <- as.integer(offset)
    end <- start + as.integer(length) - 1L
    response <- fetch_warc(filename, start, end)
    cache_warc(response, warc_cache_file_fn)
    warc <- read_warc_from_gzcon(response, include_headers)
  }
  warc
}
