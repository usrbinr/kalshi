# Order management functions for the Kalshi API

#' Create Order
#'
#' @description
#' Creates a new order in a market. Orders can be limit or market orders,
#' for either yes or no contracts.
#'
#' @details
#' Places a real, authenticated order by sending a POST request to
#' `/trade-api/v2/portfolio/orders`. The order body is assembled from `ticker`,
#' `side`, `action`, `count`, and `type`, with `yes_price`/`no_price`,
#' `expiration_ts`, `sell_position_floor`, and `buy_max_cost` added only when
#' supplied. Limit orders require either `yes_price` or `no_price`, and `side`,
#' `action`, and `type` are validated before the request is sent. On success the
#' new order's id is reported and the order object is returned.
#'
#' @param ticker Character. The market ticker.
#' @param side Character. Order side: "yes" or "no".
#' @param action Character. Order action: "buy" or "sell".
#' @param count Integer. Number of contracts.
#' @param type Character. Order type: "limit" or "market". Defaults to "limit".
#' @param yes_price Integer. Price in cents (1-99) for yes contracts. Required for limit orders.
#' @param no_price Integer. Price in cents (1-99) for no contracts. Alternative to yes_price.
#' @param expiration_ts Integer. Unix timestamp for order expiration. Optional.
#' @param sell_position_floor Integer. Minimum position to maintain when selling. Optional.
#' @param buy_max_cost Integer. Maximum cost in cents for buy orders. Optional.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A list with order details including order_id.
#'
#' @family trading
#' @export
#'
#' @examples
#' \dontrun{
#' # Buy 10 yes contracts at 45 cents
#' order <- create_order(
#'   ticker = "KXHIGHNY-25FEB08-B54",
#'   side = "yes",
#'   action = "buy",
#'   count = 10,
#'   yes_price = 45
#' )
#'
#' # Sell 5 no contracts at 60 cents
#' order <- create_order(
#'   ticker = "KXHIGHNY-25FEB08-B54",
#'   side = "no",
#'   action = "sell",
#'   count = 5,
#'   no_price = 60
#' )
#' }
create_order <- function(ticker,
                         side,
                         action,
                         count,
                         type = "limit",
                         yes_price = NULL,
                         no_price = NULL,
                         expiration_ts = NULL,
                         sell_position_floor = NULL,
                         buy_max_cost = NULL,
                         kalshi_access_token = "KALSHI_API",
                         demo = FALSE) {

    if (!side %in% c("yes", "no")) {
        cli::cli_abort("side must be 'yes' or 'no'")
    }

    if (!action %in% c("buy", "sell")) {
        cli::cli_abort("action must be 'buy' or 'sell'")
    }

    if (!type %in% c("limit", "market")) {
        cli::cli_abort("type must be 'limit' or 'market'")
    }

    if (type == "limit" && is.null(yes_price) && is.null(no_price)) {
        cli::cli_abort("Limit orders require yes_price or no_price")
    }

    body <- list(
        ticker = ticker,
        side = side,
        action = action,
        count = count,
        type = type
    )

    if (!is.null(yes_price)) body$yes_price <- yes_price
    if (!is.null(no_price)) body$no_price <- no_price
    if (!is.null(expiration_ts)) body$expiration_ts <- expiration_ts
    if (!is.null(sell_position_floor)) body$sell_position_floor <- sell_position_floor
    if (!is.null(buy_max_cost)) body$buy_max_cost <- buy_max_cost

    req <- kalshi_request(
        path = "/trade-api/v2/portfolio/orders",
        method = "POST",
        body = body,
        kalshi_access_token = kalshi_access_token,
        demo = demo
    )

    data <- kalshi_perform(req)

    cli::cli_alert_success("Order created: {data$order$order_id}")
    data$order
}

#' Cancel Order
#'
#' @description
#' Cancels an existing resting order by order ID.
#'
#' @details
#' Cancels a real, resting order by sending an authenticated DELETE request to
#' `/trade-api/v2/portfolio/orders/{order_id}`. The order id is interpolated
#' directly into the request path. On success the cancellation is reported and
#' the cancelled order object is returned.
#'
#' @param order_id Character. The order ID to cancel.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A list with the cancelled order details.
#'
#' @family trading
#' @export
#'
#' @examples
#' \dontrun{
#' cancel_order("order-uuid-here")
#' }
cancel_order <- function(order_id,
                         kalshi_access_token = "KALSHI_API",
                         demo = FALSE) {

    path <- paste0("/trade-api/v2/portfolio/orders/", order_id)

    req <- kalshi_request(
        path = path,
        method = "DELETE",
        kalshi_access_token = kalshi_access_token,
        demo = demo
    )

    data <- kalshi_perform(req)

    cli::cli_alert_success("Order cancelled: {order_id}")
    data$order
}

#' Amend Order
#'
#' @description
#' Amends an existing resting order's price and/or quantity.
#'
#' @details
#' Modifies a real, resting order by sending an authenticated POST request to
#' `/trade-api/v2/portfolio/orders/{order_id}/amend`. At least one of `count`,
#' `yes_price`, or `no_price` must be supplied, and only the provided fields are
#' included in the request body; `count` is interpreted as the new total
#' contract count. On success the amendment is reported and the amended order
#' object is returned.
#'
#' @param order_id Character. The order ID to amend.
#' @param count Integer. New total contract count. Optional.
#' @param yes_price Integer. New yes price in cents. Optional.
#' @param no_price Integer. New no price in cents. Optional.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A list with the amended order details.
#'
#' @family trading
#' @export
#'
#' @examples
#' \dontrun{
#' # Change price to 50 cents
#' amend_order("order-uuid", yes_price = 50)
#'
#' # Change quantity to 20 contracts
#' amend_order("order-uuid", count = 20)
#' }
amend_order <- function(order_id,
                        count = NULL,
                        yes_price = NULL,
                        no_price = NULL,
                        kalshi_access_token = "KALSHI_API",
                        demo = FALSE) {

    if (is.null(count) && is.null(yes_price) && is.null(no_price)) {
        cli::cli_abort("At least one of count, yes_price, or no_price must be provided")
    }

    path <- paste0("/trade-api/v2/portfolio/orders/", order_id, "/amend")

    body <- list()
    if (!is.null(count)) body$count <- count
    if (!is.null(yes_price)) body$yes_price <- yes_price
    if (!is.null(no_price)) body$no_price <- no_price

    req <- kalshi_request(
        path = path,
        method = "POST",
        body = body,
        kalshi_access_token = kalshi_access_token,
        demo = demo
    )

    data <- kalshi_perform(req)

    cli::cli_alert_success("Order amended: {order_id}")
    data$order
}

#' Decrease Order
#'
#' @description
#' Decreases the remaining quantity of a resting order. Setting to 0 cancels the order.
#'
#' @details
#' Reduces the size of a real, resting order by sending an authenticated POST
#' request to `/trade-api/v2/portfolio/orders/{order_id}/decrease` with a body
#' of `reduce_by`. This can only shrink an order, never grow it. On success the
#' reduction is reported and the updated order object is returned.
#'
#' @param order_id Character. The order ID to decrease.
#' @param reduce_by Integer. Number of contracts to reduce by.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A list with the updated order details.
#'
#' @family trading
#' @export
#'
#' @examples
#' \dontrun{
#' # Reduce order by 5 contracts
#' decrease_order("order-uuid", reduce_by = 5)
#' }
decrease_order <- function(order_id,
                           reduce_by,
                           kalshi_access_token = "KALSHI_API",
                           demo = FALSE) {

    path <- paste0("/trade-api/v2/portfolio/orders/", order_id, "/decrease")

    body <- list(reduce_by = reduce_by)

    req <- kalshi_request(
        path = path,
        method = "POST",
        body = body,
        kalshi_access_token = kalshi_access_token,
        demo = demo
    )

    data <- kalshi_perform(req)

    cli::cli_alert_success("Order decreased by {reduce_by}: {order_id}")
    data$order
}

#' Get Single Order
#'
#' @description
#' Retrieves details for a specific order by ID.
#'
#' @details
#' Looks up a single order by sending an authenticated GET request to
#' `/trade-api/v2/portfolio/orders/{order_id}`. This is a read-only operation
#' that does not modify any orders, and it returns the order object extracted
#' from the response.
#'
#' @param order_id Character. The order ID.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A list with complete order details.
#'
#' @family trading
#' @export
#'
#' @examples
#' \dontrun{
#' order <- get_order("order-uuid")
#' order$status
#' }
get_order <- function(order_id,
                      kalshi_access_token = "KALSHI_API",
                      demo = FALSE) {

    path <- paste0("/trade-api/v2/portfolio/orders/", order_id)

    req <- kalshi_request(
        path = path,
        kalshi_access_token = kalshi_access_token,
        demo = demo
    )

    data <- kalshi_perform(req)
    data$order
}

#' Batch Create Orders
#'
#' @description
#' Creates multiple orders in a single request. Limited to 20 orders per batch.
#'
#' @details
#' Places multiple real, authenticated orders in one call by sending a POST
#' request to `/trade-api/v2/portfolio/orders/batched` with the `orders` list as
#' the body. The batch is rejected locally if it contains more than 20 orders.
#' Individual orders may succeed or fail independently, so the returned list
#' contains both the created `orders` and any `errors`, with a warning emitted
#' when any errors occur.
#'
#' @param orders List of order specifications. Each order should be a list with:
#'   ticker, side, action, count, type, and optionally yes_price/no_price.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A list with created orders and any errors.
#'
#' @family trading
#' @export
#'
#' @examples
#' \dontrun{
#' orders <- list(
#'   list(
#'     ticker = "MARKET1", side = "yes", action = "buy",
#'     count = 10, type = "limit", yes_price = 45
#'   ),
#'   list(
#'     ticker = "MARKET2", side = "no", action = "buy",
#'     count = 5, type = "limit", no_price = 30
#'   )
#' )
#' result <- batch_create_orders(orders)
#' }
batch_create_orders <- function(orders,
                                kalshi_access_token = "KALSHI_API",
                                demo = FALSE) {

    if (length(orders) > 20) {
        cli::cli_abort("Batch limited to 20 orders. You provided {length(orders)}.")
    }

    body <- list(orders = orders)

    req <- kalshi_request(
        path = "/trade-api/v2/portfolio/orders/batched",
        method = "POST",
        body = body,
        kalshi_access_token = kalshi_access_token,
        demo = demo
    )

    data <- kalshi_perform(req)

    n_success <- length(data$orders)
    n_errors <- length(data$errors)

    if (n_errors > 0) {
        cli::cli_alert_warning("Created {n_success} orders with {n_errors} errors")
    } else {
        cli::cli_alert_success("Created {n_success} orders")
    }

    data
}

#' Batch Cancel Orders
#'
#' @description
#' Cancels multiple orders in a single request. Limited to 20 orders per batch.
#'
#' @details
#' Cancels multiple real, resting orders in one call by sending an authenticated
#' DELETE request to `/trade-api/v2/portfolio/orders/batched` with the
#' `order_ids` as the body. The batch is rejected locally if it contains more
#' than 20 order ids. Individual cancellations may succeed or fail
#' independently, so the returned list contains both the cancelled `orders` and
#' any `errors`, with a warning emitted when any errors occur.
#'
#' @param order_ids Character vector. Order IDs to cancel.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A list with cancelled orders and any errors.
#'
#' @family trading
#' @export
#'
#' @examples
#' \dontrun{
#' result <- batch_cancel_orders(c("order-1", "order-2", "order-3"))
#' }
batch_cancel_orders <- function(order_ids,
                                kalshi_access_token = "KALSHI_API",
                                demo = FALSE) {

    if (length(order_ids) > 20) {
        cli::cli_abort("Batch limited to 20 orders. You provided {length(order_ids)}.")
    }

    body <- list(order_ids = as.list(order_ids))

    req <- kalshi_request(
        path = "/trade-api/v2/portfolio/orders/batched",
        method = "DELETE",
        body = body,
        kalshi_access_token = kalshi_access_token,
        demo = demo
    )

    data <- kalshi_perform(req)

    n_success <- length(data$orders)
    n_errors <- length(data$errors)

    if (n_errors > 0) {
        cli::cli_alert_warning("Cancelled {n_success} orders with {n_errors} errors")
    } else {
        cli::cli_alert_success("Cancelled {n_success} orders")
    }

    data
}

#' Cancel All Orders
#'
#' @description
#' Cancels all resting orders, optionally filtered by market or event.
#'
#' @details
#' A convenience wrapper that first calls [get_orders()] to retrieve all resting
#' orders (optionally filtered by `ticker` or `event_ticker`), then cancels them
#' in groups of 20 via repeated [batch_cancel_orders()] calls. All operations
#' are authenticated and act on real orders. If there are no resting orders it
#' reports this and returns an empty tibble; otherwise it returns the tibble of
#' orders that were targeted for cancellation.
#'
#' @param ticker Character. Cancel only orders for this market ticker. Optional.
#' @param event_ticker Character. Cancel only orders for this event. Optional.
#' @param kalshi_access_token Character. Environment variable name for API key.
#' @param demo Logical. Use demo environment if TRUE.
#'
#' @return A tibble with cancelled orders.
#'
#' @family trading
#' @export
#'
#' @examples
#' \dontrun{
#' # Cancel all resting orders
#' cancel_all_orders()
#'
#' # Cancel orders for specific market
#' cancel_all_orders(ticker = "KXHIGHNY-25FEB08-B54")
#' }
cancel_all_orders <- function(ticker = NULL,
                              event_ticker = NULL,
                              kalshi_access_token = "KALSHI_API",
                              demo = FALSE) {

    # Get all resting orders
    orders <- get_orders(
        ticker = ticker,
        event_ticker = event_ticker,
        status = "resting",
        kalshi_access_token = kalshi_access_token,
        demo = demo
    )

    if (nrow(orders) == 0) {
        cli::cli_alert_info("No resting orders to cancel.")
        return(tibble::tibble())
    }

    # Cancel in batches of 20
    order_ids <- orders$order_id
    cancelled <- list()

    for (i in seq(1, length(order_ids), by = 20)) {
        batch <- order_ids[i:min(i + 19, length(order_ids))]
        result <- batch_cancel_orders(
            order_ids = batch,
            kalshi_access_token = kalshi_access_token,
            demo = demo
        )
        cancelled <- c(cancelled, result$orders)
    }

    cli::cli_alert_success("Cancelled {length(cancelled)} orders total.")

    orders
}
