if (Sys.getenv("CI") != "true") {
  Sys.setenv(http_proxy="http://proxy.gov.si:80")
  Sys.setenv(https_proxy="http://proxy.gov.si:80")
  cat("UMAR proxy is set !")
  options(continue = " ")
  if (interactive()) {
    suppressMessages(require(devtools))
    suppressMessages(require(testthat))
  }
}
