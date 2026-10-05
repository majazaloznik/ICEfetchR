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

# test
DBI::dbDisconnect(con)
dittodb::stop_db_capturing()
