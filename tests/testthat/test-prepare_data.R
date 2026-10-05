# ---- merge_history (pure) ------------------------------------------------------

test_that("merge_history with nothing stored returns fetched, sorted", {
  f <- data.frame(period_id = c("2026-10-02", "2026-10-01"), value = c(2, 1))
  m <- merge_history(NULL, f)
  expect_equal(m$data$period_id, c("2026-10-01", "2026-10-02"))
  expect_equal(m$revised, 0L)
})

test_that("merge_history keeps stored-only dates, adds new ones, ICE wins on overlap", {
  s <- data.frame(period_id = c("2026-09-29", "2026-09-30", "2026-10-01"), value = c(1, 2, 3))
  f <- data.frame(period_id = c("2026-09-30", "2026-10-01", "2026-10-02"), value = c(2, 30, 4))
  m <- merge_history(s, f)
  expect_equal(m$data$period_id, c("2026-09-29", "2026-09-30", "2026-10-01", "2026-10-02"))
  expect_equal(m$data$value, c(1, 2, 30, 4))
  expect_equal(m$revised, 1)
})

test_that("merge_history counts NA <-> value changes as revisions, NA == NA is not", {
  s <- data.frame(period_id = c("2026-09-30", "2026-10-01"), value = c(NA, NA))
  f <- data.frame(period_id = c("2026-09-30", "2026-10-01"), value = c(NA, 5))
  expect_equal(merge_history(s, f)$revised, 1)
})

# ---- nothing_new / prepare_ice_data (dittodb + mocked ICE) ---------------------

test_that("nothing_new detects whether the probes have a newer date", {
  with_mock_db({
    con <- make_test_connection()
    ids <- purrr::map_dbl(ice_series_code("GASOIL_RAW", c("202610", "202612")),
                          \(code) UMARaccessR::sql_get_series_id_from_series_code(code, con))
    probe <- data.frame(market_id = c(1, 2), series_id = ids)
    local_mocked_bindings(ice_get_history = mock_history_new)
    expect_false(nothing_new(probe, con))
    local_mocked_bindings(ice_get_history = mock_history_old)
    expect_true(nothing_new(probe, con))
  })
})

test_that("prepare_ice_data builds one vintage per updated contract, history merged", {
  local_mocked_bindings(ice_get_history = mock_history_new)
  with_mock_db({
    con <- make_test_connection()
    out <- suppressMessages(prepare_ice_data("GASOIL_RAW", test_contracts, con))
    expect_named(out, c("vintages", "insert"))
    expect_true(all(out$vintages$published == as.POSIXct("2026-10-02", tz = "UTC")))
    d <- out$insert$data
    expect_named(d, c("contract", "time", "value", "flag"))
    expect_true(all(d$contract %in% test_contracts$contract_code))
    # stored history for 202610 is kept, not just the 3 mocked days
    oct <- d[d$contract == "202610", ]
    expect_gt(nrow(oct), 3)
    expect_equal(max(oct$time), "2026-10-02")
    expect_equal(out$insert$dimension_names, "contract")
    expect_equal(out$insert$interval_id, "D")
  })
})

test_that("prepare_ice_data reports revised values", {
  local_mocked_bindings(ice_get_history = mock_history_new)
  with_mock_db({
    con <- make_test_connection()
    expect_message(prepare_ice_data("GASOIL_RAW", test_contracts, con), "revised")
  })
})

test_that("prepare_ice_data fails loudly on unregistered contracts", {
  bad <- rbind(test_contracts, data.frame(market_id = 9, strip = "Dec99",
                                          contract_code = "209912",
                                          contract_month = as.Date("2099-12-01")))
  with_mock_db({
    con <- make_test_connection()
    expect_error(prepare_ice_data("GASOIL_RAW", bad, con), "not registered: 209912")
  })
})

# ---- prepare_series_update: early exits (no DB reached) -----------------------

test_that("prepare_series_update skips contracts with no ICE history yet", {
  local_mocked_bindings(ice_get_history = function(...) data.frame(period_id = character(), value = numeric()))
  expect_message(out <- prepare_series_update("Dec29", "202912", 1, 1, con = NULL), "no history")
  expect_null(out)
})

test_that("prepare_series_update fails loudly on future dates", {
  local_mocked_bindings(ice_get_history = function(...)
    data.frame(period_id = format(Sys.Date() + 7, "%Y-%m-%d"), value = 1))
  expect_error(prepare_series_update("Dec26", "202612", 1, 1, con = NULL), "future dates")
})

# ---- read_raw_history ----------------------------------------------------------

test_that("read_raw_history fails loudly when no raw series has data", {
  local_mocked_bindings(sql_get_data_points_from_series_id = function(...) NULL,
                        .package = "UMARaccessR")
  with_mock_db({
    con <- make_test_connection()
    expect_error(read_raw_history("GASOIL_RAW", con), "No raw data stored for GASOIL_RAW")
  })
})

# ---- prepare_pos_data: the insert path -----------------------------------------

test_that("prepare_pos_data builds vintages and the insert list for new positions", {
  local_mocked_bindings(read_raw_history = function(...) raw_fixture)
  local_mocked_bindings(sql_get_data_points_from_series_id = function(...) NULL,   # nothing stored
                        .package = "UMARaccessR")
  with_mock_db({
    con <- make_test_connection()
    out <- prepare_pos_data("GASOIL_RAW", con)

    # raw_fixture only fills M01-M03; the other 21 positions are skipped
    expect_equal(nrow(out$vintages), 3)
    # published = each position's own last date: M03 only exists on 10-09
    expect_equal(sort(format(out$vintages$published, "%Y-%m-%d")),
                 c("2026-10-09", "2026-10-13", "2026-10-13"))

    d <- out$insert$data
    expect_named(d, c("position", "time", "value", "flag"))
    expect_equal(sort(unique(d$position)), c("M01", "M02", "M03"))
    expect_equal(d$value[d$position == "M01"], c(100, 101, 92))   # rolls to Nov26 on 10-13
    expect_equal(out$insert$dimension_names, "position")
    expect_length(out$insert$dimension_ids, 1)
  })
})

test_that("prepare_pos_data fails loudly when positions are not registered", {
  local_mocked_bindings(read_raw_history = function(...) raw_fixture)
  local_mocked_bindings(sql_get_series_from_table_id = function(...)
    data.frame(id = 1, code = "ICE-UMAR--GASOIL_POS--M01--D"), .package = "UMARaccessR")
  with_mock_db({
    con <- make_test_connection()
    expect_error(prepare_pos_data("GASOIL_RAW", con), "Positions not registered: M02, M03")
  })
})
