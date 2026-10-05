# ---- pure helpers (no DB) ------------------------------------------------------

test_that("helpers build codes, labels and product config", {
  expect_equal(ice_series_code("GASOIL_RAW", c("Dec26", "Jan27")),
               c("ICE--GASOIL_RAW--Dec26--D", "ICE--GASOIL_RAW--Jan27--D"))
  expect_equal(contract_label(as.Date(c("2026-12-01", "2027-01-01"))),
               c("December 2026", "January 2027"))
  expect_error(ice_product("NOPE"), "Unknown ICE product table code: NOPE")
  expect_equal(prepare_unit_table("GASOIL_RAW"), data.frame(name = "usd/t"))
})

# ---- DB-backed (dittodb fixtures) ---------------------------------------------

test_that("prepare_source_table returns NULL with a message when ICE exists", {
  with_mock_db({
    con <- make_test_connection()
    expect_message(out <- prepare_source_table(con), "already listed")
    expect_null(out)
  })
})

test_that("prepare_table_table", {
  with_mock_db({
    con <- make_test_connection()
    out <- prepare_table_table("GASOIL_RAW", con)
    expect_named(out, c("code", "name", "source_id", "url", "notes", "keep_vintage"))
    expect_equal(nrow(out), 1)
    expect_equal(out$code, "GASOIL_RAW")
    expect_true(out$keep_vintage)
    expect_equal(jsonlite::fromJSON(out$notes)$product_id, 5817)
  })
})

test_that("prepare_category_table and prepare_category_table_table", {
  with_mock_db({
    con <- make_test_connection()
    cat <- prepare_category_table(con)
    expect_named(cat, c("id", "name", "source_id"))
    expect_equal(cat$id, 0L)
    ct <- prepare_category_table_table("GASOIL_RAW", con)
    expect_named(ct, c("category_id", "table_id", "source_id"))
    expect_equal(ct$category_id, 0L)
    expect_equal(ct$source_id, cat$source_id)
  })
})

test_that("prepare_table_dimensions_table", {
  with_mock_db({
    con <- make_test_connection()
    out <- prepare_table_dimensions_table("GASOIL_RAW", con)
    expect_named(out, c("table_id", "dimension", "is_time"))
    expect_equal(out$dimension, c("contract", "date"))
    expect_equal(out$is_time, c(FALSE, TRUE))
    expect_length(unique(out$table_id), 1)
  })
})

test_that("prepare_dimension_levels_table", {
  with_mock_db({
    con <- make_test_connection()
    out <- prepare_dimension_levels_table("GASOIL_RAW", test_contracts, con)
    expect_named(out, c("tab_dim_id", "level_value", "level_text"))
    expect_equal(out$level_value, c("202610", "202612", "202912"))
    expect_equal(out$level_text, c("October 2026", "December 2026", "December 2029"))
    expect_length(unique(out$tab_dim_id), 1)
  })
})

test_that("prepare_series_table", {
  with_mock_db({
    con <- make_test_connection()
    out <- prepare_series_table("GASOIL_RAW", test_contracts, con)
    expect_named(out, c("table_id", "name_long", "unit_id", "code", "interval_id"))
    expect_equal(out$code, ice_series_code("GASOIL_RAW", test_contracts$contract_code))
    expect_true(all(out$interval_id == "D"))
    expect_false(anyNA(out$unit_id))
    expect_match(out$name_long[2], "December 2026$")
  })
})

test_that("prepare_series_levels_table derives levels from series codes", {
  with_mock_db({
    con <- make_test_connection()
    out <- prepare_series_levels_table("GASOIL_RAW", con)
    expect_named(out, c("series_id", "tab_dim_id", "level_value"))
    expect_gte(nrow(out), 3)
    expect_false(anyDuplicated(out$series_id) > 0)
    expect_true(all(grepl("^\\d{6}$", out$level_value)))
    expect_true(all(c("202610", "202612", "202912") %in% out$level_value))
    expect_length(unique(out$tab_dim_id), 1)
  })
})

test_that("unregistered table fails loudly", {
  with_mock_db({
    con <- make_test_connection()
    expect_error(get_ice_table_id("NOPE", con), "Table NOPE is not registered")
  })
})

# ---- umbrella ------------------------------------------------------------------

test_that("ICE_import_structure is idempotent", {
  local_mocked_bindings(ice_get_contracts = function(...) test_contracts)
  with_mock_db({
    con <- make_test_connection()
    res <- suppressMessages(ICE_import_structure(con = con))
    expect_named(res, c("table", "category", "category_table", "table_dimensions",
                        "dimension_levels", "unit", "series", "series_levels"))
    counts <- purrr::map_dbl(res, \(r) sum(r$count))
    expect_equal(counts, stats::setNames(rep(0, length(counts)), names(counts)))
  })
})
