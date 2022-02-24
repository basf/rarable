#' List all devices
#' @param ar A \code{\link{arable}} object. Required.
#' @param page integer; starting page of api to receive data
#' @param limit integer; number of records per page (default: 24, max 10000)
#' @param verbose logical; verbose messages on API calls
#' @rdname devices
#' @export
#' @examples
#' \dontrun{
#' ar <- arable$new(username = Sys.getenv("AR_USERNAME"),
#'   password = Sys.getenv("AR_PASSWORD"))
#' devices <- ar_devices(ar, limit = 100L)
#' ar_parse_devices(devices)
#' }
ar_devices <- function(ar, page = 1L, limit = 24L, verbose = FALSE) {
  stopifnot(!missing(ar))
  stopifnot(inherits(ar, "arable"))
  stopifnot(is.integer(page))
  stopifnot(is.integer(limit))

  qurl <- file.path(ar$base_url, "devices")

  if (verbose) {
    message("GET: ", httr::modify_url(qurl,
                                      query = list(page = page, limit = limit)))
  }

  resp <- httr::GET(qurl,
                    query = list(page = page, limit = limit),
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

  out <- parsed$items
  next_page <- ifelse(parsed$page < parsed$pages,
                      parsed$page + 1,
                      NA)

  if (is.na(next_page)) {
    # base case (no next page)
    class(out) <- append("ar_devices", class(out))
    return(out)
  }

  # recursive case (next page available)
  out <- c(
    out,
    ar_devices(ar,
      page = as.integer(next_page),
      limit = limit,
      verbose = verbose)
  )

  class(out) <- append("ar_devices", class(out))
  return(out)
}

#' parse devices into a nice data.frame
#' @rdname devices
#' @param x a devices object, as returned by [ar_devices()]
#' @export
ar_parse_devices <- function(x) {
  stopifnot(inherits(x, "ar_devices"))
  purrr::map_df(x, parse_device)
}

parse_device <- function(device) {
  dev <- device[!vapply(device, is.list, FUN.VALUE = logical(1))]

  dev[is_empty(dev)] <- NA
  devd <- as.data.frame(dev)

  out <- data.frame(
    devd,
    location_id = device$current_location$id %|||% NA,
    location_name = device$current_location$name %|||% NA,
    location_longitude = device$current_location$gps[[1]] %|||% NA,
    location_latitude = device$current_location$gps[[2]] %|||% NA,
    location_altitude = device$current_location$elev %|||% NA,
    location_tzname = device$current_location$tz_name %|||% NA,
    location_start_date = device$current_location$start_date %|||% NA
  )[, c("type", "id", "org_id", "owner_id", "name", "firmware_id",
        "reported_fw", "last_seen", "last_post", "last_deploy", "state",
        "model", "sync_interval", "batt_pct", "batt_volt", "signal_strength",
        "has_bridge", "location_id", "location_name", "location_longitude",
        "location_latitude", "location_altitude", "location_tzname",
        "location_start_date")]
  return(out)
}
