test_that("ICE_import_data_points skips all inserts when nothing is new", {
  local_mocked_bindings(prepare_ice_data = function(...) NULL)
  local_mocked_bindings(insert_new_vintage = function(...) stop("should not insert"),
                        .package = "UMARimportR")
  expect_message(out <- ICE_import_data_points(con = NULL, contracts = test_contracts),
                 "no new data")
  expect_null(out)
})

test_that("ICE_import_data_points inserts vintages, then data, inside one transaction", {
  steps <- character()
  local_mocked_bindings(prepare_ice_data = function(...)
    list(vintages = data.frame(series_id = 1), insert = list()))
  local_mocked_bindings(
    insert_new_vintage = function(...) { steps <<- c(steps, "vintage"); data.frame(count = 1) },
    insert_prepared_data_points = function(...) { steps <<- c(steps, "data"); data.frame(datapoints_inserted = 5) },
    .package = "UMARimportR")
  local_mocked_bindings(
    dbWithTransaction = function(conn, code, ...) { steps <<- c(steps, "tx"); code },
    .package = "DBI")
  res <- suppressMessages(ICE_import_data_points(con = NULL, contracts = test_contracts))
  expect_equal(steps, c("tx", "vintage", "data"))
  expect_named(res, c("vintages", "data"))
})

test_that("ICE_import_positions follows the same pattern", {
  steps <- character()
  local_mocked_bindings(prepare_pos_data = function(...)
    list(vintages = data.frame(series_id = 1), insert = list()))
  local_mocked_bindings(
    insert_new_vintage = function(...) { steps <<- c(steps, "vintage"); data.frame(count = 24) },
    insert_prepared_data_points = function(...) { steps <<- c(steps, "data"); data.frame(datapoints_inserted = 1) },
    .package = "UMARimportR")
  local_mocked_bindings(
    dbWithTransaction = function(conn, code, ...) { steps <<- c(steps, "tx"); code },
    .package = "DBI")
  expect_message(ICE_import_positions(con = NULL), "GASOIL_POS: 24 new vintage")
  expect_equal(steps, c("tx", "vintage", "data"))
})
