#' Previous business day (weekends only, no holidays)
#' @keywords internal
prev_business_day <- function(x) {
  x <- x - 1
  wd <- as.POSIXlt(x)$wday                      # 0 = Sunday, 6 = Saturday
  x - (wd == 0) * 2 - (wd == 6) * 1
}

#' First day of the month
#' @keywords internal
month_start <- function(d) as.Date(format(d, "%Y-%m-01"))

#' Add k months to first-of-month dates (vectorised over both arguments)
#' @keywords internal
add_months <- function(m, k) {
  lt <- as.POSIXlt(m)
  lt$mon <- lt$mon + k
  as.Date(lt)
}

#' Whole months from a to b (both first-of-month)
#' @keywords internal
months_between <- function(a, b) {
  a <- as.POSIXlt(a)
  b <- as.POSIXlt(b)
  (b$year - a$year) * 12L + (b$mon - a$mon)
}

#' Rule-based last trading day of an ICE contract
#'
#' @param table_code product table code
#' @param contract_month first-of-month Date(s)
#' @return Date(s)
#' @export
ice_expiry <- function(table_code, contract_month) {
  rule <- ice_product(table_code)$expiry_rule
  switch(rule,
         gasoil = prev_business_day(prev_business_day(contract_month + 13)),  # 2 bd before the 14th
         stop("Unknown expiry rule: ", rule, call. = FALSE))
}

#' Front contract month on each date
#'
#' @param dates Date vector
#' @inheritParams ice_expiry
#' @return first-of-month Date vector
#' @keywords internal
front_contract_month <- function(dates, table_code) {
  this <- month_start(dates)
  add_months(this, as.integer(dates > ice_expiry(table_code, this)))
}

#' Contract month from a YYYYMM contract code
#' @keywords internal
code_to_month <- function(contract_code) {
  m <- as.Date(paste0(contract_code, "01"), format = "%Y%m%d")
  if (anyNA(m)) stop("Unparseable contract code(s): ",
                     paste(contract_code[is.na(m)], collapse = ", "), call. = FALSE)
  m
}

#' Map raw contract prices to positions M01..Mn
#'
#' Each raw (contract, date) row maps to exactly one position:
#' position = months from the front contract month on that date, plus one.
#' Rows beyond n_positions and NA values are dropped.
#'
#' @param raw data frame: contract_code (YYYYMM), period_id (YYYY-MM-DD), value
#' @param table_code raw product table code, e.g. "GASOIL_RAW"
#' @param n_positions number of positions to keep
#' @return data frame: position ("M01".."Mnn"), period_id, value, sorted
#' @export
compute_positions <- function(raw, table_code, n_positions) {
  d  <- as.Date(raw$period_id)
  cm <- code_to_month(raw$contract_code)

  late <- d > ice_expiry(table_code, cm)
  if (any(late))
    stop("Data after rule-based expiry for: ",
         paste(unique(raw$contract_code[late]), collapse = ", "),
         ". Check the expiry rule (holidays?).", call. = FALSE)

  pos  <- months_between(front_contract_month(d, table_code), cm) + 1L
  keep <- pos <= n_positions & !is.na(raw$value)
  out  <- data.frame(position  = sprintf("M%02d", pos[keep]),
                     period_id = raw$period_id[keep],
                     value     = raw$value[keep])
  out <- out[order(out$position, out$period_id), ]
  rownames(out) <- NULL
  out
}
