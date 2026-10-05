contracts_json <- '[
  {"marketId":6227607,"marketStrip":"Oct26","endDate":1793419200000,"lastPrice":1446.0,"volume":18325,"lastTime":"09/28/2026 01:55 PM GMT","change":-1.0},
  {"marketId":6227609,"marketStrip":"Dec26","endDate":1798693200000,"lastPrice":1337.0,"volume":38606,"lastTime":"09/28/2026 01:56 PM GMT","change":-0.7},
  {"marketId":7220697,"marketStrip":"Dec29","endDate":1893387600000,"lastPrice":null,"volume":1,"lastTime":null,"change":0.0}
]'


history_json <- '{"marketId":6227609,"bars":[
  ["Fri Sep 25 00:00:00 2026",1346.25],
  ["Mon Sep 28 00:00:00 2026",1336.75],
  ["Tue Sep 29 00:00:00 2026",1297.25]
]}'

test_contracts <- data.frame(
  market_id      = c(6227607, 6227609, 7220697),
  strip          = c("Oct26", "Dec26", "Dec29"),
  contract_month = as.Date(c("2026-10-01", "2026-12-01", "2029-12-01")),
  contract_code = c("202610", "202612", "202912")
)
