devtools::load_all()
source("tests/testthat/helper-connection.R")
source("tests/testthat/helper-fixtures.R")

dittodb::start_db_capturing(path = "tests/testthat")
con <- make_test_connection()

prepare_source_table(con)
prepare_table_table("GASOIL_RAW", con)
prepare_category_table(con)
prepare_category_table_table("GASOIL_RAW", con)
prepare_table_dimensions_table("GASOIL_RAW", con)
prepare_dimension_levels_table("GASOIL_RAW", test_contracts, con)
prepare_series_table("GASOIL_RAW", test_contracts, con)
prepare_series_levels_table("GASOIL_RAW", con)
try(ICEfetchR:::get_ice_table_id("NOPE", con))
# recording script
ICE_import_structure("GASOIL_RAW", con = con, contracts = test_contracts)
ICE_import_structure("GASOIL_RAW", con = con, contracts = test_contracts)

# --- data prep (mocked ICE, read-only on the DB) ---
ids <- purrr::map_dbl(ice_series_code("GASOIL_RAW", c("202610", "202612")),
                      \(code) UMARaccessR::sql_get_series_id_from_series_code(code, con))
testthat::with_mocked_bindings({
  prepare_ice_data("GASOIL_RAW", test_contracts, con)
  nothing_new(data.frame(market_id = c(1, 2), series_id = ids), con)
}, ice_get_history = mock_history_new)
try(prepare_ice_data("GASOIL_RAW",
                     rbind(test_contracts, data.frame(market_id = 9, strip = "Dec99",
                                                      contract_code = "209912",
                                                      contract_month = as.Date("2099-12-01"))),
                     con))

# --- positions (read-only) ---
read_raw_history("GASOIL_RAW", con)
prepare_pos_table_table("GASOIL_RAW", con)
prepare_pos_dimension_levels_table("GASOIL_RAW", con)
prepare_pos_series_table("GASOIL_RAW", con)
UMARaccessR::sql_get_non_time_dimensions_from_table_id(
  get_ice_table_id("GASOIL_POS", con), con)
ICE_import_pos_structure(con = con)
ICE_import_positions(con = con)
# test
DBI::dbDisconnect(con)
dittodb::stop_db_capturing()



