if (nchar(Sys.getenv("AR_USERNAME")) == 0 ||
    nchar(Sys.getenv("AR_PASSWORD")) == 0)
  skip("No credentials for testing")

ar <- arable$new(username = Sys.getenv("AR_USERNAME"),
                 password = Sys.getenv("AR_PASSWORD"))

testthat::test_that("ar_schemas works", {
  sm <- ar_schemas(ar)
  expect_vector(sm, "character")
  expect_true("aux_raw" %in% sm)
})

testthat::test_that("ar_schema works", {
  aux <- ar_schema(ar, "aux_raw")
  expect_s3_class(aux, "data.frame")
  expect_equal(names(aux), c("column_name", "data_type", "description"))
  expect_true(nrow(aux) > 0)
})
