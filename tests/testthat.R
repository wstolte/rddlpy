
test_that("ddl_measurements works (if ddlpy present)", {
  ddl <- try(reticulate::import("ddlpy", delay_load = TRUE), silent = TRUE)
  if (inherits(ddl, "try-error")) skip("ddlpy not available")
  expect_silent(
    ddl_measurements("HOEKVHLD",
                     as.POSIXct("2023-01-01", tz="UTC"),
                     as.POSIXct("2023-01-15", tz="UTC"))
  )
})

