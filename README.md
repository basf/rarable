---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# rarable

<!-- badges: start -->
[![R build status](https://github.com/basf/rarable/workflows/R-CMD-check/badge.svg)](https://github.com/basf/rarable/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/rarable)](https://CRAN.R-project.org/package=rarable)
<!-- badges: end -->

Just an R client for [arable API](https://api.arable.cloud/api/v2/doc)
Provides access and functions to parse results into nice data.frames.

## Installation

You can install the development version of `rarable` using [`remotes::install_git`](https://remotes.r-lib.org/reference/install_git.html).

## Usage

### Create client with basic authentication

Here we read the credentials from environmental variables.


```r
library("rarable")
ar <- arable$new(username = Sys.getenv("AR_USERNAME"),
  password = Sys.getenv("AR_PASSWORD"))
ar
```

But you can also use `apikey` or `token` authentication.

### List all devices & parse to a data.frame


```r
ar %>%
  ar_devices() %>%
  ar_parse_devices() %>%
  head()
```
### List all available tables


```r
ar %>%
  ar_schemas()
```

### List all columns of a table


```r
ar %>%
  ar_schema(table = "daily") %>%
  head()
```


### Retrieve data & parse to a data.frame


```r
ar %>%
  ar_data(
    table = "daily",
    device = "C007538",
    select = c("time", "mean_rh", "meant")) %>%
  ar_parse_data() %>%
  head()
```

## Unit tests

The package comes also with a suit of tests & coverage is >75%.
However, in order to run unit tests, you need to provide credentials as
environmental variables `AR_USERNAME`and `AR_PASSWORD`.
