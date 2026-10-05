#' Import (or refresh) structural metadata for an ICE product
#'
#' Safe to run repeatedly: all inserts are ON CONFLICT DO NOTHING, so a re-run
#' only registers newly listed contracts. Run before every data import.
#'
#' @param table_code product table code, defaults to "GASOIL_RAW"
#' @param con connection to the database
#' @param schema database schema, defaults to "platform"
#' @param keep_vintage logical, defaults to TRUE
#' @param contracts optional output of [ice_get_contracts()]; fetched if NULL
#' @return list of insert results, invisibly
#' @export
ICE_import_structure <- function(table_code = "GASOIL_RAW", con, schema = "platform",
                                 keep_vintage = TRUE, contracts = NULL) {
  p <- ice_product(table_code)
  res <- list()

  source_table <- prepare_source_table(con, schema)
  if (!is.null(source_table))
    res$source <- UMARimportR::insert_new_source(con, source_table, schema)
  res$table <- UMARimportR::insert_new_table_table(
    con, prepare_table_table(table_code, con, schema, keep_vintage), schema)
  res$category <- UMARimportR::insert_new_category(
    con, prepare_category_table(con, schema), schema)
  res$category_table <- UMARimportR::insert_new_category_table(
    con, prepare_category_table_table(table_code, con, schema), schema)
  res$table_dimensions <- UMARimportR::insert_new_table_dimensions(
    con, prepare_table_dimensions_table(table_code, con, schema), schema)

  if (is.null(contracts)) contracts <- ice_get_contracts(p$product_id, p$hub_id)

  res$dimension_levels <- UMARimportR::insert_new_dimension_levels(
    con, prepare_dimension_levels_table(table_code, contracts, con, schema), schema)
  res$unit <- UMARimportR::insert_new_unit(con, prepare_unit_table(table_code), schema)
  res$series <- UMARimportR::insert_new_series(
    con, prepare_series_table(table_code, contracts, con, schema), schema)
  res$series_levels <- UMARimportR::insert_new_series_levels(
    con, prepare_series_levels_table(table_code, con, schema), schema)

  purrr::iwalk(res, \(r, nm) message(nm, ": ", sum(r$count), " new row(s)"))
  invisible(res)
}

#' Import new data points for an ICE product
#'
#' Inserts one new vintage per contract with new data, plus its full merged
#' history, in a single transaction. Follow with
#' [UMARimportR::vintage_cleanup()] to drop redundant vintages.
#'
#' @param table_code product table code, defaults to "GASOIL_RAW"
#' @param con connection to the database
#' @param schema database schema, defaults to "platform"
#' @param contracts optional output of [ice_get_contracts()]; fetched if NULL
#' @param span ICE historicalSpan, defaults to 3
#' @return list of insert results, invisibly; NULL if nothing was new
#' @export
ICE_import_data_points <- function(table_code = "GASOIL_RAW", con, schema = "platform",
                                   contracts = NULL, span = 3) {
  p <- ice_product(table_code)
  if (is.null(contracts)) contracts <- ice_get_contracts(p$product_id, p$hub_id)

  prep <- prepare_ice_data(table_code, contracts, con, schema, span)
  if (is.null(prep)) {
    message(table_code, ": no new data")
    return(invisible(NULL))
  }

  res <- DBI::dbWithTransaction(con, list(
    vintages = UMARimportR::insert_new_vintage(con, prep$vintages, schema),
    data     = UMARimportR::insert_prepared_data_points(prep$insert, con, schema)
  ))
  message(table_code, ": ", sum(res$vintages$count), " new vintage(s)")
  invisible(res)
}


#' Import (or refresh) structure for the derived position series
#'
#' Requires the raw structure (source, category) to exist. Safe to re-run.
#'
#' @param table_code raw product table code, defaults to "GASOIL_RAW"
#' @inheritParams ICE_import_structure
#' @return list of insert results, invisibly
#' @export
ICE_import_pos_structure <- function(table_code = "GASOIL_RAW", con, schema = "platform",
                                     keep_vintage = TRUE) {
  d <- ice_product(table_code)$derived_code
  res <- list()
  res$table <- UMARimportR::insert_new_table_table(
    con, prepare_pos_table_table(table_code, con, schema, keep_vintage), schema)
  res$category_table <- UMARimportR::insert_new_category_table(
    con, prepare_category_table_table(d, con, schema), schema)
  res$table_dimensions <- UMARimportR::insert_new_table_dimensions(
    con, prepare_table_dimensions_table(d, con, schema, dimension = "position"), schema)
  res$dimension_levels <- UMARimportR::insert_new_dimension_levels(
    con, prepare_pos_dimension_levels_table(table_code, con, schema), schema)
  res$series <- UMARimportR::insert_new_series(
    con, prepare_pos_series_table(table_code, con, schema), schema)
  res$series_levels <- UMARimportR::insert_new_series_levels(
    con, prepare_series_levels_table(d, con, schema, dimension = "position"), schema)
  purrr::iwalk(res, \(r, nm) message(nm, ": ", sum(r$count), " new row(s)"))
  invisible(res)
}

#' Import derived position series data
#'
#' Recomputes positions from stored raw data and inserts new vintages in a
#' single transaction. Follow with [UMARimportR::vintage_cleanup()].
#'
#' @inheritParams ICE_import_pos_structure
#' @return list of insert results, invisibly; NULL if nothing was new
#' @export
ICE_import_positions <- function(table_code = "GASOIL_RAW", con, schema = "platform") {
  prep <- prepare_pos_data(table_code, con, schema)
  if (is.null(prep)) {
    message(ice_product(table_code)$derived_code, ": no new data")
    return(invisible(NULL))
  }
  res <- DBI::dbWithTransaction(con, list(
    vintages = UMARimportR::insert_new_vintage(con, prep$vintages, schema),
    data     = UMARimportR::insert_prepared_data_points(prep$insert, con, schema)
  ))
  message(ice_product(table_code)$derived_code, ": ", sum(res$vintages$count), " new vintage(s)")
  invisible(res)
}
