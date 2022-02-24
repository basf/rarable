#' List all schemas
#' @param ar A \code{\link{arable}} object. Required.
#' @param verbose logical; verbose messages on API calls
#' @rdname schemas
#' @export
#' @examples
#' \dontrun{
#' ar <- arable$new(username = Sys.getenv("AR_USERNAME"),
#'   password = Sys.getenv("AR_PASSWORD"))
#' ar_schemas(ar)
#' }
ar_schemas <- function(ar, verbose = FALSE) {
  stopifnot(!missing(ar))
  stopifnot(inherits(ar, "arable"))

  qurl <- file.path(ar$base_url, "schemas")

  if (verbose) {
    message("GET: ", qurl)
  }

  resp <- httr::GET(qurl,
                    ar$get_auth())

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(
    httr::content(resp, "text", encoding = "UTF-8"),
    simplifyVector = FALSE)

  if (httr::status_code(resp) != 200) {
    stop(
      sprintf(
        "Arable API request failed [%s]\n%s",
        httr::status_code(resp),
        parsed$message
      ),
      call. = FALSE
    )
  }

  return(unlist(parsed))
}

#' Show schema of a table
#' @param table tablename, as returned by [ar_schemas()]
#' @rdname schemas
#' @export
#' @examples
#' \dontrun{
#' ar <- arable$new(username = Sys.getenv("AR_USERNAME"),
#'   password = Sys.getenv("AR_PASSWORD"))
#' ar_schema(ar, "daily")
#' ar_schema(ar, "hourly")
#' }
ar_schema <- function(ar, table, verbose = FALSE) {
  stopifnot(!missing(ar))
  stopifnot(inherits(ar, "arable"))
  stopifnot(!missing(table))

  qurl <- file.path(ar$base_url, "schemas", table)

  if (verbose) {
    message("GET: ", qurl)
  }

  resp <- httr::GET(qurl,
                    ar$get_auth())

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(
    httr::content(resp, "text", encoding = "UTF-8"),
    simplifyVector = FALSE)

  if (httr::status_code(resp) != 200) {
    stop(
      sprintf(
        "Arable API request failed [%s]\n%s",
        httr::status_code(resp),
        parsed$message
      ),
      call. = FALSE
    )
  }

  purrr::map_df(parsed, ~.x)
}
