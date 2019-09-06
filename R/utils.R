digest <- function(x) {
  openssl::md5(x)
}

ensure_directory_exists <- function(fn) {
  dn <- dirname(fn)
  if (dir.exists(dn)) {
    return(TRUE)
  } else {
    return(dir.create(dn, recursive = TRUE))
  }
}

#' Set Options for the ccwarcs Package
#'
#' @param cache Location for the cached CDX index results and WARC files
#' @param cdx_sleep Amount of time (seconds) to wait after a CDX server call before resuming
#' @param page_size An optional parameter to set the amount of data returned in each page
#' @return A list object of class `ccwarcs_options`
#' @examples
#' \dontrun{
#' opts <- ccwarcs_options(cache = "my_project_directory")
#' url <- "http://www.celebuzz.com/2017-01-04"
#' crawl <- "2018-47"
#' results <- get_cc_index(url, crawl, .options = opts)
#' }
#' @export
ccwarcs_options <- function(cache = "~/.ccwarcs_cache",
                            cdx_sleep = .3,
                            page_size = NULL) {
  .options <- new_ccwarcs_options(
    cache = cache,
    cdx_sleep = cdx_sleep,
    page_size = page_size
  )
  validate_ccwarcs_options(.options)
}

new_ccwarcs_options <- function(cache, cdx_sleep, page_size) {
  structure(
    list(
      cache = cache,
      cdx_sleep = cdx_sleep,
      page_size = page_size
    ),
    class = "ccwarcs_options"
  )
}

validate_ccwarcs_options <- function(.options) {
  ac <- ArgumentCheck::newArgCheck()

  if (!inherits(.options$cache, "character")) {
    ArgumentCheck::addError("cache must be a character vector", ac)
  }

  if (inherits(.options$cdx_sleep, c("numeric", "integer"))) {
    if (is.na(.options$cdx_sleep) | is.null(.options$cdx_sleep) | 
        .options$cdx_sleep <= 0) {
      ArgumentCheck::addError("cdx_sleep must be greater than zero", ac)
    }
  } else {
    ArgumentCheck::addError("cdx_sleep must be numeric", ac)
  }

  if (!is.null(.options$page_size)) {
    if (is.na(.options$page_size) ||
      as.integer(.options$page_size) != .options$page_size ||
      .options$page_size < 1) {
      ArgumentCheck::addError(
        "If provided, page_size must be a positive integer", ac)
    }
  }

  ArgumentCheck::finishArgCheck(ac)

  if (!dir.exists(path.expand(.options$cache))) {
    create_missing_dir <- utils::askYesNo(stringr::str_c(
      stringr::str_glue("The folder {.options$cache} does not exist. ",
      "Would you like to create it now?")), default = TRUE)
    if (!is.na(create_missing_dir) & create_missing_dir) {
      dir.create(path.expand(.options$cache), recursive = FALSE)
    } else {
      stop("There is no cache folder set.")
    }
  }

  .options
}
