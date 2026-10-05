test_that("gasoil expiry is 2 business days before the 14th", {
  cm <- as.Date(c("2026-10-01", "2026-11-01", "2026-12-01", "2027-02-01"))
  expect_equal(ice_expiry("GASOIL_RAW", cm),
               as.Date(c("2026-10-12", "2026-11-12", "2026-12-10", "2027-02-11")))
})

test_that("front month rolls the day after expiry", {
  d <- as.Date(c("2026-10-09", "2026-10-12", "2026-10-13", "2026-12-31"))
  expect_equal(front_contract_month(d, "GASOIL_RAW"),
               as.Date(c("2026-10-01", "2026-10-01", "2026-11-01", "2027-01-01")))
})

test_that("months_between crosses year boundaries", {
  expect_equal(months_between(as.Date("2026-11-01"), as.Date(c("2026-11-01", "2027-02-01"))),
               c(0L, 3L))
})

test_that("code_to_month parses codes and fails loudly on bad ones", {
  expect_equal(code_to_month(c("202610", "202701")), as.Date(c("2026-10-01", "2027-01-01")))
  expect_error(code_to_month(c("202610", "202613")), "Unparseable contract code\\(s\\): 202613")
})



test_that("compute_positions maps contracts to positions and rolls correctly", {
  out <- compute_positions(raw_fixture, "GASOIL_RAW", n_positions = 2)
  expect_equal(out$position,  c("M01", "M01", "M01", "M02", "M02", "M02"))
  expect_equal(out$period_id, c("2026-10-09", "2026-10-12", "2026-10-13",
                                "2026-10-09", "2026-10-12", "2026-10-13"))
  # M01: Oct26, Oct26, then Nov26 after the roll; M02: Nov26, Nov26, then Dec26
  expect_equal(out$value, c(100, 101, 92, 90, 91, 82))
})

test_that("compute_positions drops positions beyond n and NA values", {
  out <- compute_positions(raw_fixture, "GASOIL_RAW", n_positions = 3)
  # Dec26 is M03 on 10-09 (kept) and NA on 10-12 (dropped); on 10-13 it's M02
  expect_equal(out$value[out$position == "M03"], 80)
})

test_that("compute_positions fails loudly on data after expiry", {
  bad <- rbind(raw_fixture,
               data.frame(contract_code = "202610", period_id = "2026-10-13", value = 99))
  expect_error(compute_positions(bad, "GASOIL_RAW", 2), "after rule-based expiry for: 202610")
})

# ---- position structure and data (dittodb) -------------------------------------

test_that("pos helpers", {
  expect_equal(pos_codes(3), c("M01", "M02", "M03"))
  expect_equal(pos_labels(2), c("Position 1 (front month)", "Position 2"))
})

test_that("read_raw_history returns contract codes and numeric values", {
  with_mock_db({
    con <- make_test_connection()
    raw <- read_raw_history("GASOIL_RAW", con)
    expect_named(raw, c("contract_code", "period_id", "value"))
    expect_true(all(grepl("^\\d{6}$", raw$contract_code)))
    expect_type(raw$value, "double")
  })
})

test_that("prepare_pos_* build the derived table, levels and series", {
  with_mock_db({
    con <- make_test_connection()
    tt <- prepare_pos_table_table("GASOIL_RAW", con)
    expect_equal(tt$code, "GASOIL_POS")
    expect_equal(jsonlite::fromJSON(tt$notes)$derived_from, "GASOIL_RAW")

    dl <- prepare_pos_dimension_levels_table("GASOIL_RAW", con)
    expect_equal(dl$level_value, pos_codes(24))
    expect_length(unique(dl$tab_dim_id), 1)

    s <- prepare_pos_series_table("GASOIL_RAW", con)
    expect_equal(s$code[1], "ICE-UMAR--GASOIL_POS--M01--D")
    expect_equal(nrow(s), 24)
  })
})

test_that("GASOIL_POS has exactly one non-time dimension, 'position', and levels use it", {
  with_mock_db({
    con <- make_test_connection()
    dims <- UMARaccessR::sql_get_non_time_dimensions_from_table_id(
      get_ice_table_id("GASOIL_POS", con), con)
    expect_equal(dims$dimension, "position")
    expect_equal(unique(prepare_pos_dimension_levels_table("GASOIL_RAW", con)$tab_dim_id),
                 dims$id)
  })
})

test_that("ICE_import_pos_structure is idempotent and ICE_import_positions is up to date", {
  with_mock_db({
    con <- make_test_connection()
    res <- suppressMessages(ICE_import_pos_structure(con = con))
    counts <- purrr::map_dbl(res, \(r) sum(r$count))
    expect_equal(counts, stats::setNames(rep(0, length(counts)), names(counts)))
    expect_message(out <- ICE_import_positions(con = con), "no new data")
    expect_null(out)
  })
})
