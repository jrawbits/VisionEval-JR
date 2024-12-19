# https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r
# JR NOTE: Not sure we want this as it depends on httr package and we want VEBase to be "clean" of dependencies
# Example usage:
#     c(
#       "http://content.thief/",
#       "http://rud.is/this/path/does/not_exist",
#       "https://www.amazon.com/s/ref=nb_sb_noss_2?url=search-alias%3Daps&field-keywords=content+theft", 
#       "https://www.google.com/search?num=100&source=hp&ei=xGzMW5TZK6G8ggegv5_QAw&q=don%27t+be+a+content+thief&btnK=Google+Search&oq=don%27t+be+a+content+thief&gs_l=psy-ab.3...934.6243..7114...2.0..0.134.2747.26j6....2..0....1..gws-wiz.....0..0j35i39j0i131j0i20i264j0i131i20i264j0i22i30j0i22i10i30j33i22i29i30j33i160.mY7wCTYy-v0", 
#       "https://rud.is/b/2018/10/10/geojson-version-of-cbc-quebec-ridings-hex-cartograms-with-example-usage-in-r/"
#     ) -> some_urls
# 
#     check.urls <- data.frame(
#       exists = sapply(some_urls, url_exists, USE.NAMES = FALSE),
#       some_urls,
#       stringsAsFactors = FALSE
#     )
#     print(check.urls)
#' @param x a single URL
#' @param non_2xx_return_value what to do if the site exists but the
#'        HTTP status code is not in the `2xx` range. Default is to return `FALSE`.
#' @param quiet if not `FALSE`, then every time the `non_2xx_return_value` condition
#'        arises a warning message will be displayed. Default is `FALSE`.
#' @param ... other params (`timeout()` would be a good one) passed directly
#'        to `httr::HEAD()` and/or `httr::GET()`
url_exists <- function(x, non_2xx_return_value = FALSE, quiet = FALSE,...) {

  suppressPackageStartupMessages({
    require("httr", quietly = FALSE, warn.conflicts = FALSE)
  })

  # you don't need thse two functions if you're alread using `purrr`
  # but `purrr` is a heavyweight compiled pacakge that introduces
  # many other "tidyverse" dependencies and this doesnt.

  capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
    tryCatch(
      list(result = code, error = NULL),
      error = function(e) {
        if (!quiet)
          message("Error: ", e$message)

        list(result = otherwise, error = e)
      },
      interrupt = function(e) {
        stop("Terminated by user", call. = FALSE)
      }
    )
  }

  safely <- function(.f, otherwise = NULL, quiet = TRUE) {
    function(...) capture_error(.f(...), otherwise, quiet)
  }

  sHEAD <- safely(httr::HEAD)
  sGET <- safely(httr::GET)

  # Try HEAD first since it's lightweight
  res <- sHEAD(x, ...)

  if (is.null(res$result) || 
      ((httr::status_code(res$result) %/% 200) != 1)) {

    res <- sGET(x, ...)

    if (is.null(res$result)) return(NA) # or whatever you want to return on "hard" errors

    if (((httr::status_code(res$result) %/% 200) != 1)) {
      if (!quiet) warning(sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range", x))
      return(non_2xx_return_value)
    }

    return(TRUE)

  } else {
    return(TRUE)
  }

}

