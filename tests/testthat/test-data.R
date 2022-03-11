if (nchar(Sys.getenv("AR_USERNAME")) == 0 ||
    nchar(Sys.getenv("AR_PASSWORD")) == 0)
  skip("No credentials for testing")

ar <- arable$new(username = Sys.getenv("AR_USERNAME"),
                 password = Sys.getenv("AR_PASSWORD"))

d <- ar_data(ar,
             table = "daily",
             limit = 50L,
             device = "C007538",
             select = c("time", "mean_rh"),
             start_time = format(Sys.time() - 60*60*24*30*12, "%Y-%m-%dT%H:%M:%SZ"),
             end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
             )

testthat::test_that("ar_data works", {
  expect_s3_class(d, "ar_data")
  expect_true(length(d[[1]]) == 2)
  expect_equal(names(d[[1]]), c("time", "mean_rh"))
})

dp <- ar_parse_data(d)
testthat::test_that("ar_parse_data works", {
  expect_s3_class(dp, "data.frame")
  expect_equal(names(dp), c("time", "mean_rh"))
})

## TODO local_time contains local_time column
d2 <- ar_data(ar,
             table = "daily",
             limit = 50L,
             device = "C007538",
             select = c("time", "mean_rh"),
             start_time = format(Sys.time() - 60*60*24*30*12, "%Y-%m-%dT%H:%M:%SZ"),
             end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
             local_time = "America/Regina"
             ) %>% ar_parse_data()

testthat::test_that("ar_data works with local_time", {
  expect_true("local_time" %in% names(d2))
})
