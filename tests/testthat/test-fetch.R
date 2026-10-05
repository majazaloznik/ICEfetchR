# Helper: make ice_request() return parsed fixture JSON, recording its arguments
mock_ice <- function(json, env = parent.frame()) {
  calls <- new.env()
  testthat::local_mocked_bindings(
    ice_request = function(path, ...) {
      calls$path <- path
      calls$query <- list(...)
      jsonlite::fromJSON(json)
    },
    .env = env
  )
  calls
}


# ---- strip parsing -----------------------------------------------------------

test_that("ice_get_contracts parses strips and drops price fields", {
  calls <- mock_ice(contracts_json)
  res <- ice_get_contracts()
  expect_s3_class(res, "data.frame")
  expect_named(res, c("market_id", "strip", "contract_code", "contract_month"))
  expect_equal(res$strip, c("Oct26", "Dec26", "Dec29"))
  expect_equal(res$contract_month, as.Date(c("2026-10-01", "2026-12-01", "2029-12-01")))
  expect_equal(res$contract_code, c("202610", "202612", "202912"))
  expect_equal(calls$path, "contract-data")
  expect_equal(calls$query, list(productId = 5817, hubId = 9373, mode = 2))
})

test_that("ice_get_contracts strip parsing ignores the session locale", {
  old <- Sys.getlocale("LC_TIME")
  withr::defer(Sys.setlocale("LC_TIME", old))
  loc <- c("sl_SI.UTF-8", "Slovenian_Slovenia.1250")
  ok <- Filter(\(l) nzchar(suppressWarnings(Sys.setlocale("LC_TIME", l))), loc)
  testthat::skip_if(length(ok) == 0, "No Slovenian locale available")
  Sys.setlocale("LC_TIME", ok[1])
  mock_ice(contracts_json)
  expect_false(anyNA(ice_get_contracts()$contract_month))
})

test_that("ice_get_contracts fails loudly on bad responses", {
  mock_ice("[]")
  expect_error(ice_get_contracts(), "no contracts")

  mock_ice('[{"marketId":1,"endDate":1}]')
  expect_error(ice_get_contracts(), "missing field\\(s\\): marketStrip")

  mock_ice('[{"marketId":1,"marketStrip":"Dez26"}]')
  expect_error(ice_get_contracts(), "Unparseable ICE strip\\(s\\): Dez26")
})

# ---- history -----------------------------------------------------------------

test_that("ice_get_history parses bars to ISO period_ids and numeric values", {
  calls <- mock_ice(history_json)
  res <- ice_get_history(6227609)
  expect_named(res, c("period_id", "value"))
  expect_equal(res$period_id, c("2026-09-25", "2026-09-28", "2026-09-29"))
  expect_type(res$value, "double")
  expect_equal(res$value, c(1346.25, 1336.75, 1297.25))
  expect_equal(calls$path, "data/historical")
  expect_equal(calls$query, list(marketId = 6227609, historicalSpan = 3))
})

test_that("ice_get_history handles a single bar", {
  mock_ice('{"marketId":1,"bars":[["Tue Sep 29 00:00:00 2026",1297.25]]}')
  res <- ice_get_history(1)
  expect_equal(nrow(res), 1)
  expect_equal(res$period_id, "2026-09-29")
})

test_that("ice_get_history keeps null prices as NA", {
  mock_ice('{"marketId":1,"bars":[["Mon Sep 28 00:00:00 2026",null],["Tue Sep 29 00:00:00 2026",1297.25]]}')
  expect_equal(ice_get_history(1)$value, c(NA, 1297.25))
})

test_that("ice_get_history returns a typed empty frame when there are no bars", {
  mock_ice('{"marketId":1,"bars":[]}')
  res <- ice_get_history(1)
  expect_equal(nrow(res), 0)
  expect_type(res$period_id, "character")
  expect_type(res$value, "double")
})

test_that("ice_get_history fails loudly on bad dates and duplicates", {
  mock_ice('{"marketId":1,"bars":[["Tue Sept 29 00:00:00 2026",1.0]]}')
  expect_error(ice_get_history(1), "Unparseable ICE bar date")

  mock_ice('{"marketId":1,"bars":[["Tue Sep 29 00:00:00 2026",1.0],["Tue Sep 29 00:00:00 2026",2.0]]}')
  expect_error(ice_get_history(1), "Duplicate dates")
})
