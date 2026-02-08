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
