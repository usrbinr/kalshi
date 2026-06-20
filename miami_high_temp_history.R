#!/usr/bin/Rscript
# Daily Kalshi betting history for a city's "Highest temperature" market.
# For each day: the bucket ranges, the morning implied odds, and the winning bucket.

suppressMessages({
  library(kalshi)
  library(dplyr)
  library(purrr)
  library(tibble)
})

kalshi_auth_setup("/home/hagan/R/kalshi/kalshi-key.key")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# --- ticker range builder ----------------------------------------------------
# Turn a date range into Kalshi daily event tickers ("KXHIGHMIA-26JUN17"), ready
# to purrr::map() over. Returns a character vector named by ISO date.
#   kalshi_event_tickers("KXHIGHMIA", "2026-06-14", "2026-06-17")
#   #>     2026-06-14            2026-06-15            2026-06-16            2026-06-17
#   #> "KXHIGHMIA-26JUN14" "KXHIGHMIA-26JUN15" "KXHIGHMIA-26JUN16" "KXHIGHMIA-26JUN17"
kalshi_event_tickers <- function(series_ticker, from, to = from) {
  dates <- seq(as.Date(from), as.Date(to), by = "day")
  codes <- toupper(format(dates, "%y%b%d"))   # 2026-06-17 -> "26JUN17"
  purrr::set_names(paste0(series_ticker, "-", codes), as.character(dates))
}

# Just the date code, if you only want the "26JUN17" suffix:
kalshi_date_code <- function(d) toupper(format(as.Date(d), "%y%b%d"))

# --- one day -----------------------------------------------------------------
# series_ticker e.g. "KXHIGHMIA" (Miami).  Swap MIA -> LAX, TDC, etc. for other cities.
# morning_hour_utc 13 = ~9am ET; use 14 for ~9am CT, 16 for ~9am PT.
get_day_betting <- function(series_ticker, d, morning_hour_utc = 13) {

  # Kalshi event ticker: KXHIGHMIA-26JUN17
  ev <- paste0(series_ticker, "-", toupper(format(d, "%y%b%d")))

  mk <- tryCatch(as.data.frame(get_event_markets(ev)), error = function(e) NULL)
  if (is.null(mk) || !nrow(mk)) return(NULL)

  # morning candle window (1 hour around the target)
  start <- as.integer(as.POSIXct(sprintf("%s %02d:00:00", d, morning_hour_utc), tz = "UTC"))
  end   <- as.integer(as.POSIXct(sprintf("%s %02d:59:00", d, morning_hour_utc), tz = "UTC"))

  map(seq_len(nrow(mk)), function(i) {
    tk <- mk$ticker[i]
    cd <- tryCatch(
      as.data.frame(get_candlesticks(tk, series_ticker, period_interval = 60,
                                     start = start, end = end)),
      error = function(e) NULL
    )
    morning_prob <- if (!is.null(cd) && nrow(cd)) tail(cd$close, 1) else NA_real_

    tibble(
      date             = d,
      bucket           = mk$subtitle[i],                 # e.g. "93° to 94°"
      floor_f          = mk$floor_strike[i] %||% NA_real_,
      cap_f            = mk$cap_strike[i]   %||% NA_real_,
      morning_yes_prob = morning_prob,                   # implied % at the morning hour
      settle_price     = mk$last_price[i],               # ~1.00 winner, ~0.00 losers
      winner           = identical(mk$result[i], "yes")
    )
  }) |> list_rbind()
}

# --- many days ---------------------------------------------------------------
get_history <- function(series_ticker, from, to, morning_hour_utc = 13) {
  seq(as.Date(from), as.Date(to), by = "day") |>
    map(\(d) get_day_betting(series_ticker, d, morning_hour_utc)) |>
    list_rbind()
}

# ---- run --------------------------------------------------------------------
res <- get_history("KXHIGHMIA", from = "2026-06-14", to = "2026-06-17")

# winner per day + how the morning market saw it
winners <- res |>
  group_by(date) |>
  summarise(
    winning_bucket    = bucket[which(winner)][1],
    morning_prob_of_winner = morning_yes_prob[which(winner)][1],
    morning_favorite  = bucket[which.max(morning_yes_prob)],
    favorite_prob     = max(morning_yes_prob, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n================= FULL BUCKET TABLE =================\n")
print(as.data.frame(res), row.names = FALSE)
cat("\n================= WINNER vs MORNING MARKET =================\n")
print(as.data.frame(winners), row.names = FALSE)
