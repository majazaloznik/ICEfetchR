#' Merge stored and freshly fetched history for one series
#'
#' Keeps stored dates ICE no longer returns, adds new dates, and takes ICE's
#' value where both exist. Counts values that ICE changed.
#'
#' @param stored data frame (period_id, value) from the latest vintage, or NULL
#' @param fetched data frame (period_id, value) from [ice_get_history()]
#' @return list: data (period_id, value, sorted), revised (integer)
#' @keywords internal
merge_history <- function(stored, fetched) {
  if (is.null(stored) || nrow(stored) == 0) {
    fetched <- fetched[order(fetched$period_id), ]
    rownames(fetched) <- NULL
    return(list(data = fetched, revised = 0L))
  }
  m <- merge(stored[c("period_id", "value")], fetched, by = "period_id",
             all = TRUE, suffixes = c("_db", "_ice"))
  in_ice <- m$period_id %in% fetched$period_id
  both   <- in_ice & m$period_id %in% stored$period_id
  a <- m$value_db
  b <- m$value_ice
  changed <- both & ((is.na(a) != is.na(b)) | (!is.na(a) & !is.na(b) & a != b))
  list(data = data.frame(period_id = m$period_id, value = ifelse(in_ice, b, a)),
       revised = sum(changed))
}

#' TRUE if none of the probe contracts has a newer date in ICE than in the DB
#'
#' A probe series with no stored data counts as new.
#' @keywords internal
nothing_new <- function(probe, con, schema = "platform", span = 3) {
  !any(purrr::pmap_lgl(probe[c("market_id", "series_id")], \(market_id, series_id) {
    stored <- UMARaccessR::sql_get_data_points_from_series_id(con, series_id, schema = schema)
    if (is.null(stored)) return(TRUE)
    fetched <- ice_get_history(market_id, span)
    nrow(fetched) > 0 && max(fetched$period_id) > max(stored$period_id)
  }))
}

#' Prepare the new vintage and data for one contract series
#'
#' Returns NULL when ICE has no history yet or nothing newer than what's stored.
#'
#' @param strip contract strip, e.g. "Dec26"
#' @param contract_code contract code, e.g "202610"
#' @param market_id ICE marketId
#' @param series_id platform series id
#' @param con connection to the database
#' @param schema database schema, defaults to "platform"
#' @param span ICE historicalSpan
#' @return list: vintage (series_id, published), data (contract, time, value, flag), or NULL
#' @keywords internal
prepare_series_update <- function(strip, contract_code, market_id, series_id, con,
                                  schema = "platform", span = 3) {
  fetched <- ice_get_history(market_id, span)
  if (nrow(fetched) == 0) {
    message(strip, ": no history from ICE yet, skipped")
    return(NULL)
  }
  if (max(fetched$period_id) > format(Sys.Date(), "%Y-%m-%d"))
    stop(strip, ": ICE history contains future dates", call. = FALSE)

  stored <- UMARaccessR::sql_get_data_points_from_series_id(con, series_id, schema = schema)
  if (!is.null(stored) && max(fetched$period_id) <= max(stored$period_id)) return(NULL)

  merged <- merge_history(stored, fetched)
  if (merged$revised > 0)
    message(strip, ": ICE revised ", merged$revised, " stored value(s)")

  list(
    vintage = data.frame(series_id = series_id,
                         published = as.POSIXct(max(merged$data$period_id), tz = "UTC")),
    data = data.frame(contract = contract_code,
                      time = merged$data$period_id,
                      value = merged$data$value,
                      flag = "")
  )
}

#' Prepare vintages and data points for all live contracts of an ICE product
#'
#' @param table_code product table code, e.g. "GASOIL_RAW"
#' @param contracts output of [ice_get_contracts()]
#' @param con connection to the database
#' @param schema database schema, defaults to "platform"
#' @param span ICE historicalSpan
#' @return list: vintages (data frame), insert (list in the format expected by
#'   [UMARimportR::insert_prepared_data_points()]); NULL if nothing is new
#' @export
prepare_ice_data <- function(table_code, contracts, con, schema = "platform", span = 3) {
  tbl_id <- get_ice_table_id(table_code, con, schema)
  series <- UMARaccessR::sql_get_series_from_table_id(tbl_id, con, schema)
  series_contract <- vapply(strsplit(series$code, "--", fixed = TRUE), `[`, character(1), 3)

  unregistered <- setdiff(contracts$contract_code, series_contract)
  if (length(unregistered))
    stop("Contracts not registered: ", paste(unregistered, collapse = ", "),
         ". Run ICE_import_structure() first.", call. = FALSE)

  todo <- merge(contracts[c("market_id", "strip", "contract_code", "contract_month")],
                data.frame(series_id = series$id, contract_code = series_contract),
                by = "contract_code")
  todo <- todo[order(todo$contract_month), ]

  probe_rows <- if (nrow(todo) >= 3) 2:3 else seq_len(nrow(todo))
  if (nothing_new(todo[probe_rows, ], con, schema, span)) return(NULL)

  results <- purrr::pmap(todo[c("strip", "contract_code", "market_id", "series_id")],
                         \(strip, contract_code, market_id, series_id)
                         prepare_series_update(strip, contract_code, market_id, series_id, con, schema, span)) |>
    purrr::compact()

  if (length(results) == 0) return(NULL)

  list(
    vintages = purrr::map(results, "vintage") |> purrr::list_rbind(),
    insert = list(
      data            = purrr::map(results, "data") |> purrr::list_rbind(),
      table_id        = tbl_id,
      interval_id     = "D",
      dimension_ids   = get_dim_id(tbl_id, "contract", con, schema),
      dimension_names = "contract"
    )
  )
}


#' Read the latest stored history of all raw contract series
#'
#' @param table_code raw product table code
#' @inheritParams prepare_ice_data
#' @return data frame: contract_code, period_id, value
#' @keywords internal
read_raw_history <- function(table_code, con, schema = "platform") {
  tbl_id <- get_ice_table_id(table_code, con, schema)
  series <- UMARaccessR::sql_get_series_from_table_id(tbl_id, con, schema)
  codes  <- vapply(strsplit(series$code, "--", fixed = TRUE), `[`, character(1), 3)
  raw <- purrr::map2(series$id, codes, \(id, code) {
    dp <- UMARaccessR::sql_get_data_points_from_series_id(con, id, schema = schema)
    if (is.null(dp)) return(NULL)
    data.frame(contract_code = code, period_id = dp$period_id, value = as.numeric(dp$value))
  }) |> purrr::list_rbind()
  if (nrow(raw) == 0) stop("No raw data stored for ", table_code, call. = FALSE)
  raw
}

#' Prepare vintages and data points for the derived position series
#'
#' Recomputes all positions from stored raw data; only positions with a
#' newer last date than what's stored get a new vintage.
#'
#' @param table_code raw product table code, e.g. "GASOIL_RAW"
#' @inheritParams prepare_ice_data
#' @return list: vintages, insert (for insert_prepared_data_points); NULL if nothing new
#' @export
prepare_pos_data <- function(table_code, con, schema = "platform") {
  p <- ice_product(table_code)
  pos <- compute_positions(read_raw_history(table_code, con, schema), table_code, p$n_positions)

  pos_tbl <- get_ice_table_id(p$derived_code, con, schema)
  series  <- UMARaccessR::sql_get_series_from_table_id(pos_tbl, con, schema)
  series_pos <- vapply(strsplit(series$code, "--", fixed = TRUE), `[`, character(1), 3)

  unregistered <- setdiff(unique(pos$position), series_pos)
  if (length(unregistered))
    stop("Positions not registered: ", paste(unregistered, collapse = ", "),
         ". Run ICE_import_pos_structure() first.", call. = FALSE)

  results <- purrr::map2(series$id, series_pos, \(id, position) {
    new <- pos[pos$position == position, ]
    if (nrow(new) == 0) return(NULL)
    stored <- UMARaccessR::sql_get_data_points_from_series_id(con, id, schema = schema)
    if (!is.null(stored) && max(new$period_id) <= max(stored$period_id)) return(NULL)
    list(vintage = data.frame(series_id = id,
                              published = as.POSIXct(max(new$period_id), tz = "UTC")),
         data = data.frame(position = position, time = new$period_id,
                           value = new$value, flag = ""))
  }) |> purrr::compact()
  if (length(results) == 0) return(NULL)

  list(
    vintages = purrr::map(results, "vintage") |> purrr::list_rbind(),
    insert = list(
      data            = purrr::map(results, "data") |> purrr::list_rbind(),
      table_id        = pos_tbl,
      interval_id     = "D",
      dimension_ids   = get_dim_id(pos_tbl, "position", con, schema),
      dimension_names = "position"
    )
  )
}
