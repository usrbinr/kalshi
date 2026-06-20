# Historical data functions for the Kalshi API

#' Get Settled Markets
#'
#' @description
#' Retrieves settled markets with optional date filtering. Useful for
#' analyzing historical outcomes.
#'
#' @details
#' Queries the `/trade-api/v2/markets` endpoint with `status = "settled"` and
#' paginates through all matching pages (or up to `max_pages`). The `from` and
#' `to` arguments are converted to Unix-second timestamps and sent as the
#' `min_settled_ts` and `max_settled_ts` filters, so the date range applies to
#' each market's settlement time. The returned tibble flattens each market into
#' one row with its ticker identifiers, title, settlement `result`, volume,
#' open interest, and the close, expiration, and settlement times.
#'
#' @param series_ticker Character. Filter by series ticker.
#' @param event_ticker Character. Filter by event ticker.
#' @param from Date, character, or integer. Start of date range.
#'   Accepts "2026-01-01", as.Date(), or Unix seconds.
#' @param to Date, character, or integer. End of date range.
#'   Accepts "2026-02-07", as.Date(), or Unix seconds.
#' @param limit Integer. Results per page (1-1000). Defaults to 200.
#' @param max_pages Integer. Maximum pages to fetch. NULL for all.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A tibble with settled market information including results.
#'
#' @family history
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all settled markets for a series
#' results <- get_settled_markets(series_ticker = "KXHIGHNY")
#'
#' # Get markets settled in a date range
#' results <- get_settled_markets(
#'   series_ticker = "KXHIGHNY",
#'   from = "2026-01-01",
#'   to = "2026-02-01"
#' )
#'
#' # Get markets settled in the last 7 days
#' results <- get_settled_markets(from = Sys.Date() - 7)
#' }
get_settled_markets <- function(series_ticker = NULL,
                                event_ticker = NULL,
                                from = NULL,
                                to = NULL,
                                limit = 200,
                                max_pages = NULL,
                                kalshi_access_token = "KALSHI_API",
                                demo = FALSE) {

    query <- list(
        status = "settled",
        series_ticker = series_ticker,
        event_ticker = event_ticker,
        min_settled_ts = parse_timestamp(from),
        max_settled_ts = parse_timestamp(to),
        limit = limit
    )

    cli::cli_progress_step("Fetching settled markets...")

    results <- kalshi_paginate(
        path = "/trade-api/v2/markets",
        result_key = "markets",
        query = query,
        kalshi_access_token = kalshi_access_token,
        demo = demo,
        max_pages = max_pages
    )

    if (length(results) == 0) {
        cli::cli_alert_warning("No settled markets found.")
        return(tibble::tibble())
    }

    markets_tbl <- purrr::map(results, function(m) {
        tibble::tibble(
            ticker = m$ticker %||% NA_character_,
            event_ticker = m$event_ticker %||% NA_character_,
            series_ticker = m$series_ticker %||% NA_character_,
            title = m$title %||% NA_character_,
            subtitle = m$subtitle %||% NA_character_,
            result = m$result %||% NA_character_,
            volume = m$volume %||% NA_integer_,
            open_interest = m$open_interest %||% NA_integer_,
            close_time = m$close_time %||% NA_character_,
            expiration_time = m$expiration_time %||% NA_character_,
            settlement_time = m$settlement_time %||% NA_character_
        )
    }) |>
        purrr::list_rbind()

    cli::cli_alert_success("Retrieved {nrow(markets_tbl)} settled markets.")
    markets_tbl
}

#' Get Settled Events
#'
#' @description
#' Retrieves settled events with optional filtering.
#'
#' @details
#' A thin wrapper around [get_events()] that hard-codes `status = "settled"`,
#' so it returns only events that have already resolved. The `series_ticker`,
#' `limit`, `max_pages`, and authentication arguments are passed straight
#' through, and pagination continues until all settled events are retrieved or
#' `max_pages` is reached.
#'
#' @param series_ticker Character. Filter by series ticker.
#' @param limit Integer. Results per page (1-200). Defaults to 100.
#' @param max_pages Integer. Maximum pages to fetch. NULL for all.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A tibble with settled event information.
#'
#' @family history
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all settled events for a series
#' events <- get_settled_events(series_ticker = "KXHIGHNY")
#' }
get_settled_events <- function(series_ticker = NULL,
                               limit = 100,
                               max_pages = NULL,
                               kalshi_access_token = "KALSHI_API",
                               demo = FALSE) {

    get_events(
        status = "settled",
        series_ticker = series_ticker,
        limit = limit,
        max_pages = max_pages,
        kalshi_access_token = kalshi_access_token,
        demo = demo
    )
}

#' Get Series History
#'
#' @description
#' Retrieves all historical results for a series, including all settled
#' events and their market outcomes. Useful for backtesting and analysis.
#'
#' @details
#' Delegates to [get_settled_markets()] for the given `series_ticker` with no
#' date filter, so it pulls every settled market in the series (subject to
#' `max_pages`). The result is the same flattened market-level tibble produced
#' by [get_settled_markets()], one row per market with its settlement result and
#' trading statistics.
#'
#' @param series_ticker Character. The series ticker (e.g., "KXHIGHNY").
#' @param max_pages Integer. Maximum pages to fetch. NULL for all.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A tibble with all settled markets for the series.
#'
#' @family history
#' @export
#'
#' @examples
#' \dontrun{
#' # Get full history for NYC temperature series
#' history <- get_series_history("KXHIGHNY")
#' }
get_series_history <- function(series_ticker,
                               max_pages = NULL,
                               kalshi_access_token = "KALSHI_API",
                               demo = FALSE) {

    cli::cli_progress_step("Fetching history for series: {series_ticker}")

    get_settled_markets(
        series_ticker = series_ticker,
        max_pages = max_pages,
        kalshi_access_token = kalshi_access_token,
        demo = demo
    )
}

#' Get Event Results
#'
#' @description
#' Retrieves all markets and their outcomes for a specific event.
#' Accepts either a full event ticker or a series ticker with a date.
#'
#' @details
#' When `event_ticker` is `NULL`, it is constructed from `series_ticker` and
#' `date` using the Kalshi `SERIES-YYMMMDD` convention (e.g.
#' `"KXHIGHNY-26FEB07"`); supplying neither a ticker nor a series/date pair
#' raises an error. The function then fetches all markets for that event via
#' [get_event_markets()], prepends an `event_ticker` column, and returns one
#' row per market in the event.
#'
#' @param event_ticker Character. The event ticker (e.g., "KXHIGHNY-26FEB07").
#'   If NULL, series_ticker and date must be provided.
#' @param series_ticker Character. The series ticker (e.g., "KXHIGHNY").
#'   Used with date parameter.
#' @param date Date or character. The event date (e.g., "2026-02-07" or as.Date()).
#'   Used with series_ticker parameter.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A tibble with market results for the event.
#'
#' @family history
#' @export
#'
#' @examples
#' \dontrun{
#' # Using full event ticker
#' results <- get_event_results("KXHIGHNY-26FEB07")
#'
#' # Using series + date (simpler)
#' results <- get_event_results(series_ticker = "KXHIGHNY", date = "2026-02-07")
#' }
get_event_results <- function(event_ticker = NULL,
                              series_ticker = NULL,
                              date = NULL,
                              kalshi_access_token = "KALSHI_API",
                              demo = FALSE) {

    # Build event ticker from series + date if not provided
    if (is.null(event_ticker)) {
        if (is.null(series_ticker) || is.null(date)) {
            cli::cli_abort("Provide either event_ticker OR both series_ticker and date")
        }
        event_ticker <- build_event_ticker(series_ticker, date)
    }

    markets <- get_event_markets(
        event_ticker = event_ticker,
        kalshi_access_token = kalshi_access_token,
        demo = demo
    )

    if (nrow(markets) == 0) {
        return(markets)
    }

    # Add event ticker column
    markets$event_ticker <- event_ticker

    # Reorder columns
    markets <- markets[, c("event_ticker", names(markets)[names(markets) != "event_ticker"])]

    markets
}

#' Get Event Candlesticks
#'
#' @description
#' Retrieves aggregated historical OHLC data across all markets in an event.
#'
#' @details
#' Calls the `/trade-api/v2/series/{series_ticker}/events/{event_ticker}/candlesticks`
#' endpoint, building `event_ticker` from `series_ticker` and `date` when it is
#' not supplied directly. The `start` and `end` arguments are parsed to
#' Unix-second timestamps and sent as `start_ts` and `end_ts`; if omitted, the
#' window defaults to the last 30 days ending now. `period_interval` selects the
#' candle width in minutes (1, 60, or 1440). The returned tibble has one row per
#' candle, with the period-end time as a UTC `POSIXct` plus open, high, low,
#' close, and volume; request failures are caught and yield an empty tibble.
#'
#' @param event_ticker Character. The event ticker. If NULL, use series_ticker + date.
#' @param series_ticker Character. The series ticker.
#' @param date Date or character. The event date (used if event_ticker is NULL).
#' @param period_interval Integer. Candle period in minutes: 1, 60, or 1440.
#' @param start Date, character, or integer. Start date/timestamp.
#' @param end Date, character, or integer. End date/timestamp.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A tibble with OHLC data.
#'
#' @family history
#' @export
#'
#' @examples
#' \dontrun{
#' # Using series + date
#' candles <- get_event_candlesticks(
#'   series_ticker = "KXHIGHNY",
#'   date = "2026-02-07",
#'   period_interval = 60
#' )
#'
#' # Using full event ticker
#' candles <- get_event_candlesticks(
#'   event_ticker = "KXHIGHNY-26FEB07",
#'   series_ticker = "KXHIGHNY"
#' )
#' }
get_event_candlesticks <- function(event_ticker = NULL,
                                   series_ticker,
                                   date = NULL,
                                   period_interval = 60,
                                   start = NULL,
                                   end = NULL,
                                   kalshi_access_token = "KALSHI_API",
                                   demo = FALSE) {

    # Build event ticker from series + date if not provided
    if (is.null(event_ticker)) {
        if (is.null(date)) {
            cli::cli_abort("Provide either event_ticker or date")
        }
        event_ticker <- build_event_ticker(series_ticker, date)
    }

    # Parse dates to timestamps
    end_ts <- parse_timestamp(end)
    start_ts <- parse_timestamp(start)

    # Default to last 30 days if not specified
    if (is.null(end_ts)) {
        end_ts <- as.integer(Sys.time())
    }
    if (is.null(start_ts)) {
        start_ts <- end_ts - (30 * 24 * 60 * 60)
    }

    path <- paste0(
        "/trade-api/v2/series/", series_ticker,
        "/events/", event_ticker, "/candlesticks"
    )

    query <- list(
        period_interval = period_interval,
        start_ts = start_ts,
        end_ts = end_ts
    )

    req <- kalshi_request(
        path = path,
        query = query,
        kalshi_access_token = kalshi_access_token,
        demo = demo
    )

    data <- tryCatch(
        kalshi_perform(req),
        error = function(e) {
            cli::cli_alert_warning("Could not fetch candlesticks: {conditionMessage(e)}")
            return(list(candlesticks = list()))
        }
    )

    if (length(data$candlesticks) == 0) {
        return(tibble::tibble())
    }

    purrr::map(data$candlesticks, function(c) {
        tibble::tibble(
            time = as.POSIXct(c$end_period_ts, origin = "1970-01-01", tz = "UTC"),
            open = c$open_price %||% NA_integer_,
            high = c$high_price %||% NA_integer_,
            low = c$low_price %||% NA_integer_,
            close = c$close_price %||% NA_integer_,
            volume = c$volume %||% NA_integer_
        )
    }) |>
        purrr::list_rbind()
}

#' Get Market History
#'
#' @description
#' Retrieves historical trade and price data for a specific market.
#' Combines candlestick data with trade information.
#'
#' @details
#' Returns a named list that may contain a `candles` tibble and a `trades`
#' tibble. Candlesticks are fetched via [get_candlesticks()] only when
#' `include_candles` is `TRUE` and a `series_ticker` is supplied (it is required
#' by the candlesticks endpoint); failures are caught and produce an empty
#' tibble. Trades are fetched via [get_trades()] when `include_trades` is `TRUE`,
#' with `max_pages = NULL` so all available trade pages for the market are
#' pulled.
#'
#' @param ticker Character. The market ticker.
#' @param series_ticker Character. The series ticker (required for candlesticks).
#' @param include_candles Logical. Include candlestick data. Defaults to TRUE.
#' @param include_trades Logical. Include trade data. Defaults to TRUE.
#' @param period_interval Integer. Candle period in minutes: 1, 60, or 1440.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A list with candles and trades tibbles.
#'
#' @family history
#' @export
#'
#' @examples
#' \dontrun{
#' history <- get_market_history(
#'   ticker = "KXHIGHNY-25FEB08-B54",
#'   series_ticker = "KXHIGHNY"
#' )
#' history$candles
#' history$trades
#' }
get_market_history <- function(ticker,
                               series_ticker = NULL,
                               include_candles = TRUE,
                               include_trades = TRUE,
                               period_interval = 60,
                               kalshi_access_token = "KALSHI_API",
                               demo = FALSE) {

    result <- list()

    if (include_candles && !is.null(series_ticker)) {
        result$candles <- tryCatch(
            get_candlesticks(
                ticker = ticker,
                series_ticker = series_ticker,
                period_interval = period_interval,
                demo = demo
            ),
            error = function(e) {
                cli::cli_alert_warning("Could not fetch candles: {conditionMessage(e)}")
                tibble::tibble()
            }
        )
    }

    if (include_trades) {
        result$trades <- get_trades(
            ticker = ticker,
            max_pages = NULL,
            demo = demo
        )
    }

    result
}

#' Get Trade History
#'
#' @description
#' Retrieves all historical trades for a market or across all markets.
#'
#' @details
#' A wrapper around [get_trades()] that hits the `/trade-api/v2/markets/trades`
#' endpoint. When `ticker` is `NULL`, trades are returned across all markets;
#' otherwise results are filtered to that market. The `min_ts` and `max_ts`
#' bounds are passed through as-is to restrict the time window, and pagination
#' continues up to `max_pages` (default 10). The returned tibble has one row per
#' trade, including count, yes/no prices, taker side, and creation time.
#'
#' @param ticker Character. Filter by market ticker. Optional.
#' @param min_ts Integer. Minimum timestamp (Unix ms).
#' @param max_ts Integer. Maximum timestamp (Unix ms).
#' @param limit Integer. Results per page (1-1000). Defaults to 1000.
#' @param max_pages Integer. Maximum pages to fetch. Defaults to 10.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A tibble with trade information.
#'
#' @family history
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all trades for a market
#' trades <- get_trade_history(ticker = "KXHIGHNY-25FEB08-B54")
#'
#' # Get recent trades across all markets
#' trades <- get_trade_history(max_pages = 5)
#' }
get_trade_history <- function(ticker = NULL,
                              min_ts = NULL,
                              max_ts = NULL,
                              limit = 1000,
                              max_pages = 10,
                              demo = FALSE) {

    get_trades(
        ticker = ticker,
        min_ts = min_ts,
        max_ts = max_ts,
        limit = limit,
        max_pages = max_pages,
        demo = demo
    )
}
