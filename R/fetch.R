#' Build and perform a GET request to the ICE market data API
#'
#' @param path API path after `/marketdata/api/productguide/charting/`
#' @param ... query parameters
#' @return parsed JSON (list or data frame)
#' @keywords internal
ice_request <- function(path, ...) {
  httr2::request("https://www.ice.com/marketdata/api/productguide/charting/") |>
    httr2::req_url_path_append(path) |>
    httr2::req_url_query(...) |>
    httr2::req_user_agent("Mozilla/5.0") |>
    httr2::req_throttle(rate = 1, realm = "ice.com") |>          # at most 1 request/second
    httr2::req_retry(max_tries = 3, max_seconds = 60,
                     is_transient = \(resp) httr2::resp_status(resp) %in% c(500, 502, 503, 504)) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()
}


#' Get currently listed contracts for an ICE product
#'
#' Uses the contract-data endpoint only as the contract list. Its prices are
#' intraday last trades, not settlements, and are deliberately dropped.
#'
#' @param product_id ICE product id (5817 = Low Sulphur Gasoil)
#' @param hub_id ICE hub id
#' @return data frame: market_id, strip, contract_month
#' @export
ice_get_contracts <- function(product_id = 5817, hub_id = 9373) {
  out <- ice_request("contract-data", productId = product_id, hubId = hub_id, mode = 2)
  required <- c("marketId", "marketStrip")
  if (!is.data.frame(out) || nrow(out) == 0)
    stop("ICE returned no contracts for product ", product_id, call. = FALSE)
  if (length(missing <- setdiff(required, names(out))))
    stop("ICE contract list missing field(s): ", paste(missing, collapse = ", "), call. = FALSE)
  # month.abb is fixed English in R, so this doesn't depend on the Windows locale
  month <- match(substr(out$marketStrip, 1, 3), month.abb)
  year  <- 2000L + as.integer(substr(out$marketStrip, 4, 5))
  if (anyNA(month) || anyNA(year))
    stop("Unparseable ICE strip(s): ",
         paste(out$marketStrip[is.na(month) | is.na(year)], collapse = ", "), call. = FALSE)
  data.frame(
    market_id      = out$marketId,
    strip          = out$marketStrip,
    contract_code  = sprintf("%d%02d", year, month),
    contract_month = as.Date(sprintf("%d-%02d-01", year, month))
  )
}

#' Get daily price history for one ICE contract
#'
#' @param market_id ICE marketId from `ice_get_contracts()`
#' @param span ICE historicalSpan parameter (3 = ~2 years)
#' @return data frame: period_id (YYYY-MM-DD), value
#' @export
ice_get_history <- function(market_id, span = 3) {
  bars <- ice_request("data/historical", marketId = market_id, historicalSpan = span)$bars
  if (is.null(bars) || length(bars) == 0)
    return(data.frame(period_id = character(), value = numeric()))
  # bars come as a character matrix: "Tue Oct 01 00:00:00 2024", "674.5".
  # Month parsed via month.abb, not %b, so the Windows locale doesn't matter.
  parts <- strsplit(bars[, 1], " ", fixed = TRUE)
  date <- as.Date(sprintf("%s-%02d-%s",
                          vapply(parts, `[`, character(1), 5),
                          match(vapply(parts, `[`, character(1), 2), month.abb),
                          vapply(parts, `[`, character(1), 3)),
                  format = "%Y-%m-%d")
  if (anyNA(date)) stop("Unparseable ICE bar date(s) for market ", market_id, call. = FALSE)
  if (anyDuplicated(date)) stop("Duplicate dates in ICE history for market ", market_id, call. = FALSE)
  data.frame(period_id = format(date, "%Y-%m-%d"),
             value     = as.numeric(bars[, 2]))
}
