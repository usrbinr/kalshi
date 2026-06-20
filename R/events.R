# Events and Series functions for the Kalshi API

#' Get Series List
#'
#' @description
#' Retrieves all available series templates. A series represents a recurring
#' event format (e.g., "Daily High Temperature in NYC").
#'
#' @details
#' Pages through the `/trade-api/v2/series` endpoint via [kalshi_paginate()],
#' optionally narrowing results with the `category` query parameter and capping
#' the number of pages with `max_pages`. Each returned series record is flattened
#' into one row holding its ticker, title, category, frequency, fee type, contract
#' URL, and the name of its first settlement source (`NA` when none is present).
#' When no series match, a warning is emitted and an empty tibble is returned.
#'
#' @param category Character. Filter by category.
#' @param max_pages Integer. Maximum pages to fetch. NULL for all.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A tibble with series information including ticker, title, category,
#'   frequency, and settlement sources.
#'
#' @family series
#' @export
#'
#' @examples
#' \dontrun{
#' series <- get_series_list()
#' head(series)
#' }
get_series_list <- function(category = NULL,
                            max_pages = NULL,
                            kalshi_access_token = "KALSHI_API",
                            demo = FALSE) {

    query <- list(category = category)

    cli::cli_progress_step("Fetching series...")

    results <- kalshi_paginate(
        path = "/trade-api/v2/series",
        result_key = "series",
        query = query,
        kalshi_access_token = kalshi_access_token,
        demo = demo,
        max_pages = max_pages
    )

    if (length(results) == 0) {
        cli::cli_alert_warning("No series found.")
        return(tibble::tibble())
    }

    series_tbl <- purrr::map(results, function(s) {
        source_name <- if (length(s$settlement_sources) > 0) {
            s$settlement_sources[[1]]$name
        } else {
            NA_character_
        }

        tibble::tibble(
            ticker = s$ticker %||% NA_character_,
            title = s$title %||% NA_character_,
            category = s$category %||% NA_character_,
            frequency = s$frequency %||% NA_character_,
            fee_type = s$fee_type %||% NA_character_,
            source_name = source_name,
            contract_url = s$contract_url %||% NA_character_
        )
    }) |>
        purrr::list_rbind()

    cli::cli_alert_success("Retrieved {nrow(series_tbl)} series.")
    series_tbl
}

#' Get Single Series
#'
#' @description
#' Retrieves detailed information for a specific series by ticker.
#'
#' @details
#' Performs a single request to `/trade-api/v2/series/<series_ticker>` and returns
#' the raw `series` object from the response without reshaping it into a tibble.
#' Setting `include_volume = TRUE` adds the `include_volume=true` query parameter
#' so volume data is included in the response; otherwise the parameter is omitted.
#'
#' @param series_ticker Character. The series ticker (e.g., "KXHIGHNY").
#' @param include_volume Logical. Include volume data if TRUE.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A list with complete series details.
#'
#' @family series
#' @export
#'
#' @examples
#' \dontrun{
#' series <- get_series("KXHIGHNY")
#' series$title
#' series$frequency
#' }
get_series <- function(series_ticker,
                       include_volume = FALSE,
                       kalshi_access_token = "KALSHI_API",
                       demo = FALSE) {

    path <- paste0("/trade-api/v2/series/", series_ticker)

    req <- kalshi_request(
        path = path,
        query = list(include_volume = if (include_volume) "true" else NULL),
        kalshi_access_token = kalshi_access_token,
        demo = demo
    )

    data <- kalshi_perform(req)
    data$series
}

#' Get Events
#'
#' @description
#' Retrieves a list of events with optional filtering. An event is a specific
#' occurrence within a series (e.g., "NYC High Temperature on Feb 8, 2025").
#'
#' @details
#' Pages through the `/trade-api/v2/events` endpoint via [kalshi_paginate()],
#' passing `status`, `series_ticker`, and `limit` (1-200, default 100) as query
#' parameters, and adding `with_nested_markets=true` only when requested. Each
#' event is flattened into a row containing its event ticker, series ticker, title,
#' subtitle, category, status, `mutually_exclusive` flag, and strike date; the
#' nested market data itself is not unpacked into the tibble. Use `max_pages` to
#' limit how many pages are fetched, and expect an empty tibble plus a warning when
#' no events match.
#'
#' @param status Character. Filter by status: "open", "closed", "settled".
#' @param series_ticker Character. Filter by series ticker.
#' @param with_nested_markets Logical. Include nested market data.
#' @param limit Integer. Results per page (1-200). Defaults to 100.
#' @param max_pages Integer. Maximum pages to fetch. NULL for all.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A tibble with event information.
#'
#' @family events
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all open events
#' events <- get_events(status = "open")
#'
#' # Get events for a specific series
#' events <- get_events(series_ticker = "KXHIGHNY")
#' }
get_events <- function(status = NULL,
                       series_ticker = NULL,
                       with_nested_markets = FALSE,
                       limit = 100,
                       max_pages = NULL,
                       kalshi_access_token = "KALSHI_API",
                       demo = FALSE) {

    query <- list(
        status = status,
        series_ticker = series_ticker,
        with_nested_markets = if (with_nested_markets) "true" else NULL,
        limit = limit
    )

    cli::cli_progress_step("Fetching events...")

    results <- kalshi_paginate(
        path = "/trade-api/v2/events",
        result_key = "events",
        query = query,
        kalshi_access_token = kalshi_access_token,
        demo = demo,
        max_pages = max_pages
    )

    if (length(results) == 0) {
        cli::cli_alert_warning("No events found.")
        return(tibble::tibble())
    }

    events_tbl <- purrr::map(results, function(e) {
        tibble::tibble(
            event_ticker = e$event_ticker %||% NA_character_,
            series_ticker = e$series_ticker %||% NA_character_,
            title = e$title %||% NA_character_,
            subtitle = e$subtitle %||% NA_character_,
            category = e$category %||% NA_character_,
            status = e$status %||% NA_character_,
            mutually_exclusive = e$mutually_exclusive %||% NA,
            strike_date = e$strike_date %||% NA_character_
        )
    }) |>
        purrr::list_rbind()

    cli::cli_alert_success("Retrieved {nrow(events_tbl)} events.")
    events_tbl
}

#' Get Single Event
#'
#' @description
#' Retrieves detailed information for a specific event by ticker.
#'
#' @details
#' Performs a single request to `/trade-api/v2/events/<event_ticker>` and returns
#' the raw `event` object from the response without reshaping it into a tibble.
#' Setting `with_nested_markets = TRUE` adds the `with_nested_markets=true` query
#' parameter, causing the event's markets to be embedded under `$markets`;
#' otherwise the parameter is omitted and no market data is returned.
#'
#' @param event_ticker Character. The event ticker (e.g., "KXHIGHNY-25FEB08").
#' @param with_nested_markets Logical. Include nested market data.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A list with complete event details, optionally including markets.
#'
#' @family events
#' @export
#'
#' @examples
#' \dontrun{
#' event <- get_event("KXHIGHNY-25FEB08", with_nested_markets = TRUE)
#' event$title
#' event$markets
#' }
get_event <- function(event_ticker,
                      with_nested_markets = FALSE,
                      kalshi_access_token = "KALSHI_API",
                      demo = FALSE) {

    path <- paste0("/trade-api/v2/events/", event_ticker)

    req <- kalshi_request(
        path = path,
        query = list(with_nested_markets = if (with_nested_markets) "true" else NULL),
        kalshi_access_token = kalshi_access_token,
        demo = demo
    )

    data <- kalshi_perform(req)
    data$event
}

#' Get Event Markets
#'
#' @description
#' Retrieves all markets associated with a specific event.
#'
#' @details
#' Calls [get_event()] with `with_nested_markets = TRUE` and reshapes the embedded
#' `$markets` list into a tibble, with one row per market. Each row carries the
#' market ticker, title, subtitle, status, best yes bid/ask, last price, volume,
#' and settlement result. When the event has no markets, a warning is emitted and
#' an empty tibble is returned.
#'
#' @param event_ticker Character. The event ticker.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A tibble with market information for the event.
#'
#' @family events
#' @export
#'
#' @examples
#' \dontrun{
#' markets <- get_event_markets("KXHIGHNY-25FEB08")
#' }
get_event_markets <- function(event_ticker,
                              kalshi_access_token = "KALSHI_API",
                              demo = FALSE) {

    event <- get_event(
        event_ticker = event_ticker,
        with_nested_markets = TRUE,
        kalshi_access_token = kalshi_access_token,
        demo = demo
    )

    markets <- event$markets

    if (is.null(markets) || length(markets) == 0) {
        cli::cli_alert_warning("No markets found for event {event_ticker}.")
        return(tibble::tibble())
    }

    purrr::map(markets, function(m) {
        tibble::tibble(
            ticker = m$ticker %||% NA_character_,
            title = m$title %||% NA_character_,
            subtitle = m$subtitle %||% NA_character_,
            status = m$status %||% NA_character_,
            yes_bid = m$yes_bid %||% NA_integer_,
            yes_ask = m$yes_ask %||% NA_integer_,
            last_price = m$last_price %||% NA_integer_,
            volume = m$volume %||% NA_integer_,
            result = m$result %||% NA_character_
        )
    }) |>
        purrr::list_rbind()
}


#' Build Event Tickers for a Date Range
#'
#' @description
#' Builds a vector of Kalshi daily event tickers spanning a date range, ready to
#' iterate over with [purrr::map()]. Each ticker has the form `SERIES-YYMMMDD`
#' (e.g., `KXHIGHMIA-26JUN17`).
#'
#' @details
#' This is the vectorised companion to the single-date ticker builder: it
#' expands `from`..`to` to one ticker per day and names the result by ISO date,
#' so the names survive a [purrr::map()] / [purrr::imap()] and keep each result
#' tagged with its day. `to` defaults to `from`, so a single date returns one
#' ticker. Both dates are checked with [awh::validate_date()], so malformed or
#' impossible dates (e.g. `"2026-02-31"`) error early rather than silently
#' producing bad tickers.
#'
#' @param series_ticker Character. The series ticker (e.g., `"KXHIGHMIA"`).
#' @param from A `Date` or `"yyyy-mm-dd"` string. First date in the range
#'   (inclusive).
#' @param to A `Date` or `"yyyy-mm-dd"` string. Last date in the range
#'   (inclusive). Defaults to `from`, giving a single day.
#'
#' @return A named character vector of event tickers, named by ISO date.
#'
#' @family events
#' @export
#'
#' @examples
#' \dontrun{
#' tickers <- build_event_tickers("KXHIGHMIA", "2026-06-14", "2026-06-17")
#'
#' # one buckets-table per day, combined
#' purrr::map(tickers, get_event_markets) |> purrr::list_rbind()
#' }
build_event_tickers <- function(series_ticker, from, to = from) {
    from_valid <- awh::validate_date(as.character(from))
    to_valid <- awh::validate_date(as.character(to))

    if (is.na(from_valid)) {
        cli::cli_abort("{.arg from} ({from}) is not a valid {.val yyyy-mm-dd} date.")
    }
    if (is.na(to_valid)) {
        cli::cli_abort("{.arg to} ({to}) is not a valid {.val yyyy-mm-dd} date.")
    }

    from <- as.Date(from_valid)
    to <- as.Date(to_valid)

    if (to < from) {
        cli::cli_abort("{.arg to} ({to}) must be on or after {.arg from} ({from}).")
    }

    dates <- seq(from, to, by = "day")

    purrr::set_names(
        purrr::map_chr(dates, \(d) build_event_ticker(series_ticker, d)),
        as.character(dates)
    )
}
