if (nchar(Sys.getenv("AR_USERNAME")) == 0 ||
    nchar(Sys.getenv("AR_PASSWORD")) == 0)
  skip("No credentials for testing")

ar <- arable$new(username = Sys.getenv("AR_USERNAME"),
                 password = Sys.getenv("AR_PASSWORD"))

dev <- ar_devices(ar)
testthat::test_that("ar_devices works", {
  expect_s3_class(dev, "list")
  expect_s3_class(dev, "ar_devices")
})

testthat::test_that("ar_parse_devices works", {
  dev_p <- ar_parse_devices(dev)
  expect_s3_class(dev_p, "data.frame")
  expect_true(all(c("id", "type", "name", "location_id") %in% names(dev_p)))
})
