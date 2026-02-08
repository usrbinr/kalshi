# Portfolio functions for the Kalshi API

#' Get Account Balance
#'
#' @description
#' Retrieves your current account balance and portfolio value.
#' Values are returned in cents.
#'
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A list with balance information:
#' \itemize{
#'   \item \code{balance}: Available balance in cents
#'   \item \code{portfolio_value}: Current portfolio value in cents
#' }
#'
#' @family portfolio
#' @export
#'
#' @examples
#' \dontrun{
#' bal <- get_balance()
#' cat("Balance: $", bal$balance / 100, "\n")
#' }
get_balance <- function(kalshi_access_token = "KALSHI_API",
                        demo = FALSE) {

    req <- kalshi_request(
        path = "/trade-api/v2/portfolio/balance",
        kalshi_access_token = kalshi_access_token,
        demo = demo
    )

    kalshi_perform(req)
}

#' Get Positions
#'
#' @description
#' Retrieves your current positions across all markets.
#'
#' @param ticker Character. Filter by specific market ticker.
#' @param event_ticker Character. Filter by event ticker.
#' @param limit Integer. Results per page (1-1000). Defaults to 100.
#' @param max_pages Integer. Maximum pages to fetch. NULL for all.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A tibble with position information including market ticker,
#'   position size, average price, and realized/unrealized P&L.
#'
#' @family portfolio
#' @export
#'
#' @examples
#' \dontrun{
#' positions <- get_positions()
#' }
get_positions <- function(ticker = NULL,
                          event_ticker = NULL,
                          limit = 100,
                          max_pages = NULL,
                          kalshi_access_token = "KALSHI_API",
                          demo = FALSE) {

    query <- list(
        ticker = ticker,
        event_ticker = event_ticker,
        limit = limit
    )

    results <- kalshi_paginate(
        path = "/trade-api/v2/portfolio/positions",
        result_key = "positions",
        query = query,
        kalshi_access_token = kalshi_access_token,
        demo = demo,
        max_pages = max_pages
    )

    if (length(results) == 0) {
        return(tibble::tibble())
    }

    purrr::map(results, function(p) {
        tibble::tibble(
            ticker = p$ticker %||% NA_character_,
            event_ticker = p$event_ticker %||% NA_character_,
            position = p$position %||% NA_integer_,
            market_exposure = p$market_exposure %||% NA_integer_,
            realized_pnl = p$realized_pnl %||% NA_integer_,
            total_traded = p$total_traded %||% NA_integer_,
            resting_orders_count = p$resting_orders_count %||% NA_integer_
        )
    }) |>
        purrr::list_rbind()
}

#' Get Orders
#'
#' @description
#' Retrieves your orders with optional filtering.
#'
#' @param ticker Character. Filter by specific market ticker.
#' @param event_ticker Character. Filter by event ticker.
#' @param status Character. Filter by status: "resting", "canceled", "executed".
#' @param limit Integer. Results per page (1-200). Defaults to 100.
#' @param max_pages Integer. Maximum pages to fetch. NULL for all.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A tibble with order information.
#'
#' @family portfolio
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all resting orders
#' orders <- get_orders(status = "resting")
#'
#' # Get orders for specific market
#' orders <- get_orders(ticker = "KXHIGHNY-25FEB08-B54")
#' }
get_orders <- function(ticker = NULL,
                       event_ticker = NULL,
                       status = NULL,
                       limit = 100,
                       max_pages = NULL,
                       kalshi_access_token = "KALSHI_API",
                       demo = FALSE) {

    query <- list(
        ticker = ticker,
        event_ticker = event_ticker,
        status = status,
        limit = limit
    )

    results <- kalshi_paginate(
        path = "/trade-api/v2/portfolio/orders",
        result_key = "orders",
        query = query,
        kalshi_access_token = kalshi_access_token,
        demo = demo,
        max_pages = max_pages
    )

    if (length(results) == 0) {
        return(tibble::tibble())
    }

    purrr::map(results, function(o) {
        tibble::tibble(
            order_id = o$order_id %||% NA_character_,
            ticker = o$ticker %||% NA_character_,
            event_ticker = o$event_ticker %||% NA_character_,
            type = o$type %||% NA_character_,
            side = o$side %||% NA_character_,
            status = o$status %||% NA_character_,
            yes_price = o$yes_price %||% NA_integer_,
            no_price = o$no_price %||% NA_integer_,
            count = o$count %||% NA_integer_,
            remaining_count = o$remaining_count %||% NA_integer_,
            created_time = o$created_time %||% NA_character_,
            expiration_time = o$expiration_time %||% NA_character_
        )
    }) |>
        purrr::list_rbind()
}

#' Get Fills
#'
#' @description
#' Retrieves your trade fills (executed trades).
#'
#' @param ticker Character. Filter by specific market ticker.
#' @param order_id Character. Filter by specific order ID.
#' @param limit Integer. Results per page (1-1000). Defaults to 100.
#' @param max_pages Integer. Maximum pages to fetch. NULL for all.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A tibble with fill information.
#'
#' @family portfolio
#' @export
#'
#' @examples
#' \dontrun{
#' fills <- get_fills()
#' }
get_fills <- function(ticker = NULL,
                      order_id = NULL,
                      limit = 100,
                      max_pages = NULL,
                      kalshi_access_token = "KALSHI_API",
                      demo = FALSE) {

    query <- list(
        ticker = ticker,
        order_id = order_id,
        limit = limit
    )

    results <- kalshi_paginate(
        path = "/trade-api/v2/portfolio/fills",
        result_key = "fills",
        query = query,
        kalshi_access_token = kalshi_access_token,
        demo = demo,
        max_pages = max_pages
    )

    if (length(results) == 0) {
        return(tibble::tibble())
    }

    purrr::map(results, function(f) {
        tibble::tibble(
            trade_id = f$trade_id %||% NA_character_,
            order_id = f$order_id %||% NA_character_,
            ticker = f$ticker %||% NA_character_,
            side = f$side %||% NA_character_,
            type = f$type %||% NA_character_,
            yes_price = f$yes_price %||% NA_integer_,
            no_price = f$no_price %||% NA_integer_,
            count = f$count %||% NA_integer_,
            is_taker = f$is_taker %||% NA,
            created_time = f$created_time %||% NA_character_
        )
    }) |>
        purrr::list_rbind()
}

#' Get Settlements
#'
#' @description
#' Retrieves your settlement history.
#'
#' @param ticker Character. Filter by specific market ticker.
#' @param event_ticker Character. Filter by event ticker.
#' @param limit Integer. Results per page (1-1000). Defaults to 100.
#' @param max_pages Integer. Maximum pages to fetch. NULL for all.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A tibble with settlement information.
#'
#' @family portfolio
#' @export
#'
#' @examples
#' \dontrun{
#' settlements <- get_settlements()
#' }
get_settlements <- function(ticker = NULL,
                            event_ticker = NULL,
                            limit = 100,
                            max_pages = NULL,
                            kalshi_access_token = "KALSHI_API",
                            demo = FALSE) {

    query <- list(
        ticker = ticker,
        event_ticker = event_ticker,
        limit = limit
    )

    results <- kalshi_paginate(
        path = "/trade-api/v2/portfolio/settlements",
        result_key = "settlements",
        query = query,
        kalshi_access_token = kalshi_access_token,
        demo = demo,
        max_pages = max_pages
    )

    if (length(results) == 0) {
        return(tibble::tibble())
    }

    purrr::map(results, function(s) {
        tibble::tibble(
            ticker = s$ticker %||% NA_character_,
            event_ticker = s$event_ticker %||% NA_character_,
            market_result = s$market_result %||% NA_character_,
            position = s$position %||% NA_integer_,
            revenue = s$revenue %||% NA_integer_,
            settled_time = s$settled_time %||% NA_character_
        )
    }) |>
        purrr::list_rbind()
}
