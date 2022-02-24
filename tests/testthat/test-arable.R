testthat::test_that("arabvle fails with missing credentials", {
  expect_error(arable$new())
  arable$new(apikey = "")
})

if (nchar(Sys.getenv("AR_USERNAME")) == 0 ||
    nchar(Sys.getenv("AR_PASSWORD")) == 0)
  skip("No credentials for testing")


ar <- arable$new(username = Sys.getenv("AR_USERNAME"),
                 password = Sys.getenv("AR_PASSWORD"))

testthat::test_that("arable works", {
  expect_s3_class(ar, "R6")
  expect_s3_class(ar, "arable")
  expect_equal(ar$base_url, "https://api.arable.cloud/api/v2")
  expect_s3_class(ar$get_auth(), "request")
  expect_true("Authorization" %in% names(ar$get_auth()$headers))
  expect_true(ar$ping())
})

if (nchar(Sys.getenv("AR_APIKEY")) == 0)
  skip("No credentials for testing")

apk <- arable$new(apikey = Sys.getenv("AR_APIKEY"))

testthat::test_that("arable works with APIKEY", {
  expect_s3_class(apk, "R6")
  expect_s3_class(apk, "arable")
  expect_equal(apk$base_url, "https://api.arable.cloud/api/v2")
  expect_s3_class(apk$get_auth(), "request")
  expect_true("Authorization" %in% names(apk$get_auth()$headers))
  expect_true(apk$ping())
})
