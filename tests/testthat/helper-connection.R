library(testthat)
library(dittodb)

make_test_connection <- function() {
  DBI::dbConnect(RPostgres::Postgres(),
                 dbname = "production_backup",
                 host = "192.168.38.21",
                 port = 5432,
                 user = "postgres",
                 password = Sys.getenv("PG_PG_PSW"),
                 client_encoding = "utf8")
}
