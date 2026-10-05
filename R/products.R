#' ICE products tracked by this package
#'
#' One row per product. `table_code` is both the platform table code and the
#' second segment of the series codes (ICE--<table_code>--<strip>--D).
#'
#' @return data frame of product configuration
#' @keywords internal
ice_products <- function() {
  data.frame(
    table_code = "GASOIL_RAW",
    product_id = 5817,
    hub_id     = 9373,
    name       = "ICE Low Sulphur Gasoil Futures",
    unit       = "usd/t",
    url        = "https://www.ice.com/products/34361119/Low-Sulphur-Gasoil-Futures",
    expiry_rule = "gasoil",
    derived_code = "GASOIL_POS",
    n_positions = 24
  )
}

#' Look up one ICE product by table code
#'
#' @param table_code e.g. "GASOIL_RAW"
#' @return single-row data frame
#' @keywords internal
ice_product <- function(table_code) {
  p <- ice_products()
  row <- p[p$table_code == table_code, ]
  if (nrow(row) != 1) stop("Unknown ICE product table code: ", table_code, call. = FALSE)
  row
}

#' Build raw ICE series codes
#' @keywords internal
ice_series_code <- function(table_code, strip) {
  paste("ICE", table_code, strip, "D", sep = "--")
}

#' Human-readable contract label, locale-independent ("December 2026")
#' @keywords internal
contract_label <- function(contract_month) {
  paste(month.name[as.integer(format(contract_month, "%m"))], format(contract_month, "%Y"))
}
