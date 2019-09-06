
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ccwarcs

<!-- badges: start -->

<!-- badges: end -->

Provides access to Common Crawl WARC files via Amazon Web
Services.

## Installation

<!-- You can install the released version of ccwarcs from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r --> <!-- install.packages("ccwarcs") --> <!-- ``` -->

You can install the development version of ccwarcs from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jasonmtroos/ccwarcs")
```

## Prerequisites

This package will not work unless you are signed up for Amazon Web
Services (AWS) and have generated the appropriate security credentials.

To obtain an AWS account, open the [AWS home
page](https://portal.aws.amazon.com/gp/aws/developer/registration/index.html)
and then click Sign Up.

After you have created an AWS account, use the [Identity and Access
Management (IAM) console](https://console.aws.amazon.com/iam/home#/home)
to create a user. On the Permissions tab for the new user, grant it the
`AmazonS3ReadOnlyAccess` policy. On the Security credentials tab, create
a new access key.

To make the new security credentials available to the ccwarcs package,
you will typically take one of two actions:

1.  Set the environment variables (‘AWS\_ACCESS\_KEY\_ID’,
    ‘AWS\_SECRET\_ACCESS\_KEY’, ‘AWS\_DEFAULT\_REGION’, and
    ‘AWS\_SESSION\_TOKEN’)

2.  Install and configure the [AWS Command Line
    Interface](https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-welcome.html)

Additional options are documented at the [home page for the
aws.signature
package](https://github.com/cloudyr/aws.signature/blob/master/README.md).

## Using ccwarcs

Load the library.

``` r
library(ccwarcs)
```

Test whether AWS credentials can be located.

``` r
test_AWS_credentials()
#> Locating credentials
#> Checking for credentials in user-supplied values
#> Checking for credentials in Environment Variables
#> Using Environment Variable 'AWS_ACCESS_KEY_ID' for AWS Access Key ID
#> Using Environment Variable 'AWS_SECRET_ACCESS_KEY' for AWS Secret Access Key
#> Using default value for AWS Region ('us-east-1')
#> AWS credentials were found.
```

Create a new `ccwarcs_options` object with default values. If the
directory `~/.ccwarcs_cache` does not already exist, the package will
ask if you would like to create it.

``` r
opts <- ccwarcs_options()
opts
#> $cache
#> [1] "~/.ccwarcs_cache"
#> 
#> $cdx_sleep
#> [1] 0.3
#> 
#> $page_size
#> NULL
#> 
#> attr(,"class")
#> [1] "ccwarcs_options"
```

The `cdx_sleep` option determines the number of seconds to wait between
calls to the Common Crawl Index Server. Values smaller than the default
of `0.3` may cause the index server to refuse your requests.

The `cache` option determines where calls to the index server and web
page archives will be cached on the local file system. Accessing the
index server and downloading WARCs over the internet is slow. The
ccwarcs package minimizes downloads by caching both of these.

The function `cdx_fetch_list_of_crawls` will retrieve the current,
complete list of available crawl archives. Avoid calling this function
too often.

``` r
list_of_crawls <- cdx_fetch_list_of_crawls()
list_of_crawls %>%
  dplyr::filter(stringr::str_detect(id, '2019'))
#> # A tibble: 8 x 2
#>   id      name               
#>   <chr>   <chr>              
#> 1 2019-35 August 2019 Index  
#> 2 2019-30 July 2019 Index    
#> 3 2019-26 June 2019 Index    
#> 4 2019-22 May 2019 Index     
#> 5 2019-18 April 2019 Index   
#> 6 2019-13 March 2019 Index   
#> 7 2019-09 February 2019 Index
#> 8 2019-04 January 2019 Index
```

Next, retrive index information about the URL
[r-project.org](https://r-project.org) from the `2019-35` crawl.

``` r
cc_index <- 
  get_cc_index(urls = "r-project.org", crawls = "2019-35", .options = opts)
```

The data returned corresponds with the following call to the index
server:
<https://index.commoncrawl.org/CC-MAIN-2019-35-index?url=r-project.org&output=json>

The index information is held in a data frame.

``` r
colnames(cc_index)
#>  [1] "urlkey"        "timestamp"     "mime"          "digest"       
#>  [5] "charset"       "mime-detected" "status"        "length"       
#>  [9] "offset"        "filename"      "url"           "languages"
```

A web page archive (WARC) is uniquely described by the values in four
columns.

``` r
cc_index %>%
  dplyr::select(filename, offset, length, digest) %>%
  dplyr::glimpse()
#> Observations: 3
#> Variables: 4
#> $ filename <chr> "crawl-data/CC-MAIN-2019-35/segments/1566027317516.88/w…
#> $ offset   <int> 938342027, 937381688, 11505169
#> $ length   <int> 3019, 3019, 503
#> $ digest   <chr> "ILB6S7TS5WMLJVJIUBRQA53XRK2I3DN7", "ILB6S7TS5WMLJVJIUB…
```

Choose one of the archived versions of the `r-project.org` web page to
retrieve from the archive.

``` r
target_warc_index <- 
  cc_index %>%
  dplyr::filter(status == "200") %>%
  dplyr::filter(timestamp == max((timestamp)))
target_warc_index %>%
  dplyr::glimpse()
#> Observations: 1
#> Variables: 12
#> $ urlkey          <chr> "org,r-project)/"
#> $ timestamp       <chr> "20190824083925"
#> $ mime            <chr> "text/html"
#> $ digest          <chr> "ILB6S7TS5WMLJVJIUBRQA53XRK2I3DN7"
#> $ charset         <chr> "UTF-8"
#> $ `mime-detected` <chr> "text/html"
#> $ status          <int> 200
#> $ length          <int> 3019
#> $ offset          <int> 937381688
#> $ filename        <chr> "crawl-data/CC-MAIN-2019-35/segments/15660273199…
#> $ url             <chr> "https://www.r-project.org/"
#> $ languages       <chr> "eng"
```

Use the `get_warc` function to obtain the archived web page.

``` r
warc <- 
  target_warc_index %>%
  dplyr::mutate(warc = get_warc(filename, offset, length, digest, 
                                include_headers = FALSE, .options  = opts)) %>%
  magrittr::extract2('warc')
stringr::str_trunc(warc, width = 300) %>%
  cat()
#> <!DOCTYPE html>
#> <html lang="en">
#>   <head>
#>     <meta charset="utf-8">
#>     <meta http-equiv="X-UA-Compatible" content="IE=edge">
#>     <meta name="viewport" content="width=device-width, initial-scale=1">
#>     <title>R: The R Project for Statistical Computing</title>
#> 
#>     <link rel="icon" type="image/p...
```

The web page archive can be converted into an HTML object and
manipulated using the `rvest` package.

``` r
warc %>%
  rvest::minimal_html() %>%
  rvest::html_nodes('#news + ul li p') %>%
  rvest::html_text()
#> [1] "R version 3.6.1 (Action of the Toes) has been released on 2019-07-05."                                                                                         
#> [2] "useR! 2020 will take place in St. Louis, Missouri, USA."                                                                                                       
#> [3] "R version 3.5.3 (Great Truth) has been released on 2019-03-11."                                                                                                
#> [4] "The R Foundation Conference Committee has released a call for proposals to host useR! 2020 in North America."                                                  
#> [5] "You can now support the R Foundation with a renewable subscription as a supporting member"                                                                     
#> [6] "The R Foundation has been awarded the Personality/Organization of the year 2018 award by the professional association of German market and social researchers."
```

## Contributing

Please note that the ‘ccwarcs’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
