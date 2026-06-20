# Utility functions for the kalshi package

# Null-coalescing operator (internal, not documented)
`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

#' Build Event Ticker from Series and Date
#'
#' @description
#' Constructs a Kalshi event ticker from a series ticker and date.
#' Format: SERIES-YYMMMDD (e.g., KXHIGHNY-26FEB07)
#'
#' @details
#' Character and POSIXt inputs are first coerced to `Date` via `as.Date()`;
#' a `Date` is used as-is. The date is then formatted as `%y%b%d` and
#' upper-cased to produce the two-digit year, three-letter month abbreviation,
#' and two-digit day, which is appended to the series ticker after a hyphen.
#' Month names follow the current locale, so a non-English locale may yield an
#' invalid ticker.
#'
#' @param series_ticker Character. The series ticker.
#' @param date Date, character, or POSIXt. The event date.
#'
#' @return Character. The constructed event ticker.
#' @keywords internal
build_event_ticker <- function(series_ticker, date) {
    # Convert to Date if needed
    if (is.character(date)) {
        date <- as.Date(date)
    } else if (inherits(date, "POSIXt")) {
        date <- as.Date(date)
    }

    # Format: YYMMMDD (e.g., 26FEB07)
    date_part <- toupper(format(date, "%y%b%d"))

    paste0(series_ticker, "-", date_part)
}

#' Parse Date Input to Unix Timestamp
#'
#' @description
#' Converts various date inputs to Unix timestamp (seconds).
#'
#' @details
#' A `NULL` input returns `NULL`. Numeric input is assumed to already be a Unix
#' timestamp and is returned via `as.integer()` (truncated, not rounded).
#' Character input is parsed with `as.Date()` first. The resulting `Date` or
#' `POSIXt` value is converted with `as.POSIXct()` and coerced to an integer
#' number of seconds; this conversion uses the session's local time zone.
#'
#' @param x Date, character, POSIXt, or numeric (already Unix timestamp).
#'
#' @return Integer. Unix timestamp in seconds.
#' @keywords internal
parse_timestamp <- function(x) {
    if (is.null(x)) return(NULL)

    if (is.numeric(x)) {
        # Already a timestamp
        return(as.integer(x))
    }

    if (is.character(x)) {
        x <- as.Date(x)
    }

    as.integer(as.POSIXct(x))
}

#' Build a Kalshi API Request
#'
#' @description
#' Internal helper to construct an authenticated httr2 request.
#'
#' @details
#' Builds the base URL via `kalshi_base_url(demo = demo)` and sets the path and
#' HTTP method. Any `NULL`-valued query parameters are dropped before being
#' added, and a JSON body is attached only when `body` is supplied. The request
#' is then signed through `req_kalshi_auth()` and configured to retry up to
#' three times via `httr2::req_retry()`. The returned request is not yet
#' performed.
#'
#' @param path Character. The API endpoint path (e.g., "/trade-api/v2/markets").
#' @param method Character. HTTP method. Defaults to "GET".
#' @param query Named list. Query parameters to append.
#' @param body List. Request body for POST/PUT requests.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return An httr2 request object ready to perform.
#' @keywords internal
kalshi_request <- function(path,
                           method = "GET",
                           query = NULL,
                           body = NULL,
                           kalshi_access_token = "KALSHI_API",
                           demo = FALSE) {
    base_url <- kalshi_base_url(demo = demo)

    req <- httr2::request(base_url) |>
        httr2::req_url_path(path) |>
        httr2::req_method(method)

    if (!is.null(query)) {
        query <- Filter(Negate(is.null), query)
        if (length(query) > 0) {
            req <- httr2::req_url_query(req, !!!query)
        }
    }

    if (!is.null(body)) {
        req <- httr2::req_body_json(req, body)
    }

    req |>
        req_kalshi_auth(kalshi_access_token = kalshi_access_token) |>
        httr2::req_retry(max_tries = 3)
}

#' Perform a Kalshi API Request
#'
#' @description
#' Internal helper to execute a request and parse the JSON response.
#'
#' @details
#' Calls `httr2::req_perform()` on the request and returns the parsed body via
#' `httr2::resp_body_json()`. Retry behaviour is inherited from the request
#' object (configured in `kalshi_request()`); HTTP error statuses raise an
#' `httr2` error rather than being returned.
#'
#' @param req An httr2 request object.
#'
#' @return Parsed JSON response as a list.
#' @keywords internal
kalshi_perform <- function(req) {
    resp <- httr2::req_perform(req)
    httr2::resp_body_json(resp)
}

#' Paginate Through Kalshi API Results
#'
#' @description
#' Internal helper to handle cursor-based pagination.
#'
#' @details
#' Repeatedly builds and performs requests via `kalshi_request()` and
#' `kalshi_perform()`, appending the elements under `result_key` from each
#' response into a single flat list. The `cursor` from each response is passed
#' as a query parameter to the next request. Pagination stops when the cursor
#' is `NULL` or an empty string, or once `max_pages` pages have been fetched
#' (when `max_pages` is non-`NULL`).
#'
#' @param path Character. The API endpoint path.
#' @param result_key Character. The key in the response containing results.
#' @param query Named list. Additional query parameters.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#' @param max_pages Integer. Maximum number of pages to fetch. NULL for all.
#'
#' @return A list of all results across pages.
#' @keywords internal
kalshi_paginate <- function(path,
                            result_key,
                            query = NULL,
                            kalshi_access_token = "KALSHI_API",
                            demo = FALSE,
                            max_pages = NULL) {
    all_results <- list()
    cursor <- NULL
    page <- 0

    repeat {
        page <- page + 1

        current_query <- c(query, list(cursor = cursor))

        req <- kalshi_request(
            path = path,
            query = current_query,
            kalshi_access_token = kalshi_access_token,
            demo = demo
        )

        resp <- kalshi_perform(req)

        results <- resp[[result_key]]
        if (!is.null(results)) {
            all_results <- c(all_results, results)
        }

        cursor <- resp$cursor
        if (is.null(cursor) || cursor == "") break
        if (!is.null(max_pages) && page >= max_pages) break
    }

    all_results
}
