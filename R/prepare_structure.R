#' TRUE if a lookup returned nothing (UMARaccessR uses both NULL and NA)
#' @keywords internal
id_missing <- function(id) is.null(id) || length(id) == 0 || is.na(id[1])

#' Get the ICE source id, failing loudly if ICE is not registered
#' @keywords internal
get_ice_source_id <- function(con, schema = "platform") {
  id <- UMARaccessR::sql_get_source_code_from_source_name(con, "ICE", schema)
  if (id_missing(id)) stop("ICE is not in the source table; run prepare_source_table() first", call. = FALSE)
  id
}

#' Get a table id, failing loudly if the table is not registered
#' @keywords internal
get_ice_table_id <- function(table_code, con, schema = "platform") {
  id <- UMARaccessR::sql_get_table_id_from_table_code(con, table_code, schema)
  if (id_missing(id)) stop("Table ", table_code, " is not registered", call. = FALSE)
  id
}


#' Get the tab_dim_id of a named non-time dimension
#' @keywords internal
get_dim_id <- function(tbl_id, dimension, con, schema = "platform") {
  dims <- UMARaccessR::sql_get_non_time_dimensions_from_table_id(tbl_id, con, schema)
  id <- dims$id[dims$dimension == dimension]
  if (length(id) != 1) stop("Expected one '", dimension, "' dimension for table id ", tbl_id, call. = FALSE)
  id
}

#' Prepare row for the `source` table
#'
#' @param con connection to the database
#' @param schema database schema, defaults to "platform"
#' @return data frame with id, name, name_long, url, or NULL if ICE already exists
#' @export
prepare_source_table <- function(con, schema = "platform") {
  if (!id_missing(UMARaccessR::sql_get_source_code_from_source_name(con, "ICE", schema))) {
    message("ICE already listed in the source table.")
    return(NULL)
  }
  max_id <- DBI::dbGetQuery(con, sprintf("SELECT max(id) AS max FROM %s.source",
                                         DBI::dbQuoteIdentifier(con, schema)))$max
  data.frame(id = max_id + 1, name = "ICE", name_long = "ICE Futures Europe",
             url = "https://www.ice.com")
}

#' Prepare row for the `table` table
#'
#' @param table_code product table code, e.g. "GASOIL_RAW"
#' @param keep_vintage logical, defaults to TRUE (redundant vintages are removed
#'   by hash cleanup; revised history is kept)
#' @inheritParams prepare_source_table
#' @return data frame with code, name, source_id, url, notes, keep_vintage
#' @export
prepare_table_table <- function(table_code, con, schema = "platform", keep_vintage = TRUE) {
  p <- ice_product(table_code)
  notes <- jsonlite::toJSON(list(product_id = p$product_id, hub_id = p$hub_id,
                                 api = "marketdata/api/productguide/charting"),
                            auto_unbox = TRUE)
  data.frame(code = p$table_code, name = p$name,
             source_id = get_ice_source_id(con, schema), url = p$url,
             notes = as.character(notes), keep_vintage = keep_vintage)
}

#' Prepare row for the `category` table (single root category per source)
#'
#' @inheritParams prepare_source_table
#' @return data frame with id, name, source_id
#' @export
prepare_category_table <- function(con, schema = "platform") {
  data.frame(id = 0L, name = "ICE Futures Europe", source_id = get_ice_source_id(con, schema))
}

#' Prepare row for the `category_table` table
#'
#' @inheritParams prepare_table_table
#' @return data frame with category_id, table_id, source_id
#' @export
prepare_category_table_table <- function(table_code, con, schema = "platform") {
  data.frame(category_id = 0L,
             table_id = get_ice_table_id(table_code, con, schema),
             source_id = get_ice_source_id(con, schema))
}

#' Prepare rows for the `table_dimensions` table
#'
#' One non-time dimension plus the time dimension `date`.
#'
#' @param table_code platform table code, e.g. "GASOIL_RAW" or "GASOIL_POS"
#' @param con connection to the database
#' @param schema database schema, defaults to "platform"
#' @param dimension name of the non-time dimension, defaults to "contract"
#' @return data frame with table_id, dimension, is_time
#' @export
prepare_table_dimensions_table <- function(table_code, con, schema = "platform",
                                           dimension = "contract") {
  data.frame(table_id = get_ice_table_id(table_code, con, schema),
             dimension = c(dimension, "date"),
             is_time = c(FALSE, TRUE))
}

#' Prepare rows for the `dimension_levels` table
#'
#' @param contracts output of [ice_get_contracts()]
#' @inheritParams prepare_table_table
#' @return data frame with tab_dim_id, level_value, level_text
#' @export
prepare_dimension_levels_table <- function(table_code, contracts, con, schema = "platform") {
  tbl_id <- get_ice_table_id(table_code, con, schema)
  data.frame(tab_dim_id = get_dim_id(tbl_id, "contract", con, schema),
             level_value = contracts$contract_code,
             level_text = contract_label(contracts$contract_month))
}

#' Prepare row for the `unit` table
#'
#' @inheritParams prepare_table_table
#' @return data frame with name
#' @export
prepare_unit_table <- function(table_code) {
  data.frame(name = ice_product(table_code)$unit)
}

#' Prepare rows for the `series` table, one per contract
#'
#' @inheritParams prepare_dimension_levels_table
#' @return data frame with table_id, name_long, unit_id, code, interval_id
#' @export
prepare_series_table <- function(table_code, contracts, con, schema = "platform") {
  p <- ice_product(table_code)
  unit_id <- UMARaccessR::sql_get_unit_id_from_unit_name(p$unit, con, schema)
  if (is.null(unit_id) || is.na(unit_id))
    stop("Unit '", p$unit, "' is not in the unit table", call. = FALSE)
  data.frame(table_id = get_ice_table_id(table_code, con, schema),
             name_long = paste0(p$name, " - ", contract_label(contracts$contract_month)),
             unit_id = unit_id,
             code = ice_series_code(table_code, contracts$contract_code),
             interval_id = "D")
}

#' Prepare rows for the `series_levels` table
#'
#' Derives the level of the non-time dimension from the third segment of each
#' series code (e.g. "202612" from ICE--GASOIL_RAW--202612--D, or "M01" from
#' ICE-UMAR--GASOIL_POS--M01--D).
#'
#' @param table_code platform table code, e.g. "GASOIL_RAW" or "GASOIL_POS"
#' @param con connection to the database
#' @param schema database schema, defaults to "platform"
#' @param dimension name of the non-time dimension, defaults to "contract"
#' @return data frame with series_id, tab_dim_id, level_value
#' @export
prepare_series_levels_table <- function(table_code, con, schema = "platform",
                                        dimension = "contract") {
  tbl_id <- get_ice_table_id(table_code, con, schema)
  series <- UMARaccessR::sql_get_series_from_table_id(tbl_id, con, schema)
  data.frame(series_id = series$id,
             tab_dim_id = get_dim_id(tbl_id, dimension, con, schema),
             level_value = vapply(strsplit(series$code, "--", fixed = TRUE), `[`, character(1), 3))
}

#' Position codes and labels
#' @keywords internal
pos_codes  <- function(n) sprintf("M%02d", seq_len(n))
#' @keywords internal
pos_labels <- function(n) ifelse(seq_len(n) == 1, "Position 1 (front month)",
                                 paste("Position", seq_len(n)))

#' Prepare row for the derived positions `table` table
#'
#' @param table_code raw product table code, e.g. "GASOIL_RAW"
#' @inheritParams prepare_table_table
#' @return data frame with code, name, source_id, url, notes, keep_vintage
#' @export
prepare_pos_table_table <- function(table_code, con, schema = "platform", keep_vintage = TRUE) {
  p <- ice_product(table_code)
  notes <- jsonlite::toJSON(list(
    derived_from = p$table_code,
    method = paste("Mk = front contract month + (k-1) months;",
                   "front = earliest contract whose last trading day is on or after the date"),
    expiry_rule = p$expiry_rule,
    n_positions = p$n_positions), auto_unbox = TRUE)
  data.frame(code = p$derived_code,
             name = sprintf("%s - positions M01-M%02d", p$name, p$n_positions),
             source_id = get_ice_source_id(con, schema), url = p$url,
             notes = as.character(notes), keep_vintage = keep_vintage)
}

#' Prepare rows for the positions `dimension_levels` table
#'
#' @inheritParams prepare_pos_table_table
#' @return data frame with tab_dim_id, level_value, level_text
#' @export
prepare_pos_dimension_levels_table <- function(table_code, con, schema = "platform") {
  p <- ice_product(table_code)
  tbl_id <- get_ice_table_id(p$derived_code, con, schema)
  data.frame(tab_dim_id = get_dim_id(tbl_id, "position", con, schema),
             level_value = pos_codes(p$n_positions),
             level_text = pos_labels(p$n_positions))
}

#' Prepare rows for the positions `series` table
#'
#' @inheritParams prepare_pos_table_table
#' @return data frame with table_id, name_long, unit_id, code, interval_id
#' @export
prepare_pos_series_table <- function(table_code, con, schema = "platform") {
  p <- ice_product(table_code)
  unit_id <- UMARaccessR::sql_get_unit_id_from_unit_name(p$unit, con, schema)
  if (id_missing(unit_id)) stop("Unit '", p$unit, "' is not in the unit table", call. = FALSE)
  data.frame(table_id = get_ice_table_id(p$derived_code, con, schema),
             name_long = paste0(p$name, " - ", tolower(pos_labels(p$n_positions))),
             unit_id = unit_id,
             code = paste("ICE-UMAR", p$derived_code, pos_codes(p$n_positions), "D", sep = "--"),
             interval_id = "D")
}
