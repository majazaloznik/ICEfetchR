test_that("live ICE endpoints still have the expected shape", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  testthat::skip_if_offline("www.ice.com")

  k <- ice_get_contracts()
  expect_gt(nrow(k), 12)                       # at least a year of monthly contracts
  expect_false(anyNA(k$contract_month))
  expect_false(anyDuplicated(k$strip) > 0)

  h <- ice_get_history(k$market_id[1])
  expect_gt(nrow(h), 200)
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", h$period_id)))
  expect_lt(as.Date(max(h$period_id)), Sys.Date() + 1)   # no future dates
  expect_false(any(weekdays(as.Date(h$period_id), abbreviate = FALSE) %in%
                     weekdays(as.Date(c("2026-10-03", "2026-10-04")), abbreviate = FALSE)))  # no weekends
})
