test_that("gasoil expiry is 2 business days before the 14th", {
  cm <- as.Date(c("2026-10-01", "2026-11-01", "2026-12-01", "2027-02-01"))
  expect_equal(ice_expiry("GASOIL_RAW", cm),
               as.Date(c("2026-10-12", "2026-11-12", "2026-12-10", "2027-02-11")))
})

test_that("front month rolls the day after expiry", {
  d <- as.Date(c("2026-10-09", "2026-10-12", "2026-10-13", "2026-12-31"))
  expect_equal(front_contract_month(d, "GASOIL_RAW"),
               as.Date(c("2026-10-01", "2026-10-01", "2026-11-01", "2027-01-01")))
})

test_that("months_between crosses year boundaries", {
  expect_equal(months_between(as.Date("2026-11-01"), as.Date(c("2026-11-01", "2027-02-01"))),
               c(0L, 3L))
})

test_that("code_to_month parses codes and fails loudly on bad ones", {
  expect_equal(code_to_month(c("202610", "202701")), as.Date(c("2026-10-01", "2027-01-01")))
  expect_error(code_to_month(c("202610", "202613")), "Unparseable contract code\\(s\\): 202613")
})

raw_fixture <- data.frame(
  contract_code = rep(c("202610", "202611", "202612"), c(2, 3, 3)),
  period_id     = c("2026-10-09", "2026-10-12",
                    "2026-10-09", "2026-10-12", "2026-10-13",
                    "2026-10-09", "2026-10-12", "2026-10-13"),
  value         = c(100, 101, 90, 91, 92, 80, NA, 82)
)

test_that("compute_positions maps contracts to positions and rolls correctly", {
  out <- compute_positions(raw_fixture, "GASOIL_RAW", n_positions = 2)
  expect_equal(out$position,  c("M01", "M01", "M01", "M02", "M02", "M02"))
  expect_equal(out$period_id, c("2026-10-09", "2026-10-12", "2026-10-13",
                                "2026-10-09", "2026-10-12", "2026-10-13"))
  # M01: Oct26, Oct26, then Nov26 after the roll; M02: Nov26, Nov26, then Dec26
  expect_equal(out$value, c(100, 101, 92, 90, 91, 82))
})

test_that("compute_positions drops positions beyond n and NA values", {
  out <- compute_positions(raw_fixture, "GASOIL_RAW", n_positions = 3)
  # Dec26 is M03 on 10-09 (kept) and NA on 10-12 (dropped); on 10-13 it's M02
  expect_equal(out$value[out$position == "M03"], 80)
})

test_that("compute_positions fails loudly on data after expiry", {
  bad <- rbind(raw_fixture,
               data.frame(contract_code = "202610", period_id = "2026-10-13", value = 99))
  expect_error(compute_positions(bad, "GASOIL_RAW", 2), "after rule-based expiry for: 202610")
})
