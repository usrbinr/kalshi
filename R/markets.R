# Market data functions for the Kalshi API

#' Get Exchange Status
#'
#' @description
#' Retrieves the current operational status of the Kalshi exchange.
#'
#' @details
#' Performs a single GET request to the \code{/trade-api/v2/exchange/status}
#' endpoint and returns the parsed response list unmodified. No authentication
#' is required and no pagination is involved. This is useful for confirming the
#' exchange and trading engine are up before placing orders.
#'
#' @param demo Logical. Use demo environment if TRUE. Defaults to FALSE.
#'
#' @return A list with exchange status information:
#' \itemize{
#'   \item \code{exchange_active}: Whether the exchange is operational
#'   \item \code{trading_active}: Whether trading is currently enabled
#' }
#'
#' @family exchange
#' @export
#'
#' @examples
#' \dontrun{
#' status <- get_exchange_status()
#' status$trading_active
#' }
get_exchange_status <- function(demo = FALSE) {
    req <- kalshi_request("/trade-api/v2/exchange/status", demo = demo)
    kalshi_perform(req)
}

#' Get Markets
#'
#' @description
#' Retrieves a list of markets with optional filtering. Handles pagination
#' automatically to return all matching results.
#'
#' @details
#' Queries the \code{/trade-api/v2/markets} endpoint, passing \code{status},
#' \code{series_ticker}, \code{event_ticker}, \code{tickers} (collapsed into a
#' comma-separated string), and \code{limit} as query parameters. Results are
#' fetched page by page via cursor-based pagination until exhausted or
#' \code{max_pages} is reached, and each market is flattened into one row of a
#' tibble containing identifiers, status, bid/ask and last prices, volumes,
#' open interest, result, and close/expiration times. An empty tibble is
#' returned (with a warning) when no markets match.
#'
#' @param status Character. Filter by status: "unopened", "open", "closed", "settled".
#' @param series_ticker Character. Filter by series ticker.
#' @param event_ticker Character. Filter by event ticker.
#' @param tickers Character vector. Specific market tickers to retrieve.
#' @param limit Integer. Results per page (1-1000). Defaults to 100.
#' @param max_pages Integer. Maximum pages to fetch. NULL for all. Defaults to NULL.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A tibble with market information including ticker, title, status,
#'   yes_price, no_price, volume, and other market details.
#'
#' @family markets
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all open markets
#' markets <- get_markets(status = "open")
#'
#' # Get markets for a specific series
#' markets <- get_markets(series_ticker = "KXHIGHNY")
#' }
get_markets <- function(status = NULL,
                        series_ticker = NULL,
                        event_ticker = NULL,
                        tickers = NULL,
                        limit = 100,
                        max_pages = NULL,
                        kalshi_access_token = "KALSHI_API",
                        demo = FALSE) {

    query <- list(
        status = status,
        series_ticker = series_ticker,
        event_ticker = event_ticker,
        tickers = if (!is.null(tickers)) paste(tickers, collapse = ",") else NULL,
        limit = limit
    )

    cli::cli_progress_step("Fetching markets...")

    results <- kalshi_paginate(
        path = "/trade-api/v2/markets",
        result_key = "markets",
        query = query,
        kalshi_access_token = kalshi_access_token,
        demo = demo,
        max_pages = max_pages
    )

    if (length(results) == 0) {
        cli::cli_alert_warning("No markets found matching criteria.")
        return(tibble::tibble())
    }

    markets_tbl <- purrr::map(results, function(m) {
        tibble::tibble(
            ticker = m$ticker %||% NA_character_,
            event_ticker = m$event_ticker %||% NA_character_,
            series_ticker = m$series_ticker %||% NA_character_,
            title = m$title %||% NA_character_,
            subtitle = m$subtitle %||% NA_character_,
            status = m$status %||% NA_character_,
            yes_bid = m$yes_bid %||% NA_integer_,
            yes_ask = m$yes_ask %||% NA_integer_,
            no_bid = m$no_bid %||% NA_integer_,
            no_ask = m$no_ask %||% NA_integer_,
            last_price = m$last_price %||% NA_integer_,
            volume = m$volume %||% NA_integer_,
            volume_24h = m$volume_24h %||% NA_integer_,
            open_interest = m$open_interest %||% NA_integer_,
            result = m$result %||% NA_character_,
            close_time = m$close_time %||% NA_character_,
            expiration_time = m$expiration_time %||% NA_character_
        )
    }) |>
        purrr::list_rbind()

    cli::cli_alert_success("Retrieved {nrow(markets_tbl)} markets.")
    markets_tbl
}

#' Get Single Market
#'
#' @description
#' Retrieves detailed information for a specific market by ticker.
#'
#' @details
#' Performs a single GET request to \code{/trade-api/v2/markets/\{ticker\}} and
#' returns the \code{market} element of the response. Unlike
#' \code{\link{get_markets}}, the result is the raw parsed list rather than a
#' tibble, and no pagination occurs.
#'
#' @param ticker Character. The market ticker (e.g., "KXHIGHNY-25FEB08-B54").
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A list with complete market details.
#'
#' @family markets
#' @export
#'
#' @examples
#' \dontrun{
#' market <- get_market("KXHIGHNY-25FEB08-B54")
#' market$title
#' }
get_market <- function(ticker,
                       kalshi_access_token = "KALSHI_API",
                       demo = FALSE) {

    path <- paste0("/trade-api/v2/markets/", ticker)
    req <- kalshi_request(
        path = path,
        kalshi_access_token = kalshi_access_token,
        demo = demo
    )
    data <- kalshi_perform(req)
    data$market
}

#' Get Market Orderbook
#'
#' @description
#' Retrieves the current order book for a specific market showing bid depths.
#'
#' @details
#' Performs a single GET request to
#' \code{/trade-api/v2/markets/\{ticker\}/orderbook}, passing \code{depth} as a
#' query parameter to limit the number of price levels returned, and returns the
#' \code{orderbook} element of the response. The result is a list whose
#' \code{yes} and \code{no} elements hold the resting bid levels for each side.
#'
#' @param ticker Character. The market ticker.
#' @param depth Integer. Order book depth (0 or negative for all levels, 1-100 for specific depth).
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A list with yes and no order book levels.
#'
#' @family markets
#' @export
#'
#' @examples
#' \dontrun{
#' book <- get_orderbook("KXHIGHNY-25FEB08-B54")
#' book$yes
#' }
get_orderbook <- function(ticker,
                          depth = 10,
                          kalshi_access_token = "KALSHI_API",
                          demo = FALSE) {

    path <- paste0("/trade-api/v2/markets/", ticker, "/orderbook")
    req <- kalshi_request(
        path = path,
        query = list(depth = depth),
        kalshi_access_token = kalshi_access_token,
        demo = demo
    )
    data <- kalshi_perform(req)
    data$orderbook
}

#' Get Market Trades
#'
#' @description
#' Retrieves recent trades for all markets or a specific market.
#'
#' @details
#' Queries the \code{/trade-api/v2/markets/trades} endpoint with \code{ticker},
#' \code{limit}, and the \code{min_ts}/\code{max_ts} Unix-millisecond timestamp
#' filters as query parameters. Results are gathered via cursor-based
#' pagination, but \code{max_pages} defaults to 1 so only the first page is
#' fetched unless raised. Each trade is shaped into one row of a tibble with
#' ticker, trade_id, count, yes/no price, taker_side, and created_time; an empty
#' tibble is returned when there are no matching trades.
#'
#' @param ticker Character. Optional market ticker to filter trades.
#' @param limit Integer. Number of trades to return (1-1000). Defaults to 100.
#' @param min_ts Integer. Minimum timestamp (Unix ms) filter.
#' @param max_ts Integer. Maximum timestamp (Unix ms) filter.
#' @param max_pages Integer. Maximum pages to fetch. NULL for all.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A tibble with trade information.
#'
#' @family markets
#' @export
#'
#' @examples
#' \dontrun{
#' # Get recent trades
#' trades <- get_trades(limit = 50)
#'
#' # Get trades for specific market
#' trades <- get_trades(ticker = "KXHIGHNY-25FEB08-B54")
#' }
get_trades <- function(ticker = NULL,
                       limit = 100,
                       min_ts = NULL,
                       max_ts = NULL,
                       max_pages = 1,
                       demo = FALSE) {

    query <- list(
        ticker = ticker,
        limit = limit,
        min_ts = min_ts,
        max_ts = max_ts
    )

    results <- kalshi_paginate(
        path = "/trade-api/v2/markets/trades",
        result_key = "trades",
        query = query,
        demo = demo,
        max_pages = max_pages
    )

    if (length(results) == 0) {
        return(tibble::tibble())
    }

    purrr::map(results, function(t) {
        tibble::tibble(
            ticker = t$ticker %||% NA_character_,
            trade_id = t$trade_id %||% NA_character_,
            count = t$count %||% NA_integer_,
            yes_price = t$yes_price %||% NA_integer_,
            no_price = t$no_price %||% NA_integer_,
            taker_side = t$taker_side %||% NA_character_,
            created_time = t$created_time %||% NA_character_
        )
    }) |>
        purrr::list_rbind()
}

#' Get Market Candlesticks
#'
#' @description
#' Retrieves historical OHLC price data for a specific market.
#'
#' @details
#' Queries the
#' \code{/trade-api/v2/series/\{series_ticker\}/markets/\{ticker\}/candlesticks}
#' endpoint with \code{period_interval} (1, 60, or 1440 minutes) plus
#' \code{start_ts} and \code{end_ts} as Unix-second timestamps. The \code{start}
#' and \code{end} arguments are parsed from dates, character strings, or Unix
#' seconds; when omitted, \code{end} defaults to now and \code{start} to 30 days
#' earlier. Errors from the request are caught and surfaced as a warning,
#' yielding an empty tibble. Each candlestick is flattened into a row with a
#' UTC time (from \code{end_period_ts}), OHLC values nested under \code{price},
#' volume, open interest, and yes bid/ask open/close prices.
#'
#' @param ticker Character. The market ticker.
#' @param series_ticker Character. The series ticker for this market.
#' @param period_interval Integer. Candle period in minutes: 1, 60, or 1440.
#' @param start Date, character, or integer. Start date/timestamp.
#'   Accepts "2026-02-01", as.Date(), or Unix seconds. Defaults to 30 days ago.
#' @param end Date, character, or integer. End date/timestamp.
#'   Accepts "2026-02-07", as.Date(), or Unix seconds. Defaults to now.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A tibble with OHLC data: time, open, high, low, close, volume.
#'
#' @family markets
#' @export
#'
#' @examples
#' \dontrun{
#' # Using dates (simple)
#' candles <- get_candlesticks(
#'   ticker = "KXHIGHNY-26FEB07-B26.5",
#'   series_ticker = "KXHIGHNY",
#'   start = "2026-02-06",
#'   end = "2026-02-08"
#' )
#'
#' # Using period interval
#' candles <- get_candlesticks(
#'   ticker = "KXHIGHNY-26FEB07-B26.5",
#'   series_ticker = "KXHIGHNY",
#'   period_interval = 1  # 1-minute candles
#' )
#' }
get_candlesticks <- function(ticker,
                             series_ticker,
                             period_interval = 60,
                             start = NULL,
                             end = NULL,
                             demo = FALSE) {

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

    path <- paste0("/trade-api/v2/series/", series_ticker, "/markets/", ticker, "/candlesticks")

    query <- list(
        period_interval = period_interval,
        start_ts = start_ts,
        end_ts = end_ts
    )

    req <- kalshi_request(path = path, query = query, demo = demo)

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
        # Price data is nested under $price
        price <- c$price %||% list()
        yes_bid <- c$yes_bid %||% list()
        yes_ask <- c$yes_ask %||% list()
        tibble::tibble(
            time = as.POSIXct(c$end_period_ts, origin = "1970-01-01", tz = "UTC"),
            open = as.numeric(price$open %||% NA),
            high = as.numeric(price$high %||% NA),
            low = as.numeric(price$low %||% NA),
            close = as.numeric(price$close %||% NA),
            volume = as.numeric(c$volume %||% NA),
            open_interest = as.numeric(c$open_interest %||% NA),
            yes_bid_open = as.numeric(yes_bid$open %||% NA),
            yes_bid_close = as.numeric(yes_bid$close %||% NA),
            yes_ask_open = as.numeric(yes_ask$open %||% NA),
            yes_ask_close = as.numeric(yes_ask$close %||% NA)
        )
    }) |>
        purrr::list_rbind()
}
