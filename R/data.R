#' Get data
#' @param ar A \code{\link{arable}} object. Required.
#' @param table character; tablename, as returned by [ar_schemas()]. Required.
#' @param cursor cursor to start retrieval from
#' @param limit integer; limit; (1-10000)
#' @param order character; order
#' @param temp character; temp unit to use
#' @param pres chracter; pressure unit
#' @param ratio character; ratio unit
#' @param size character; size unit
#' @param speed character; speed unit
#' @param vol charcter; volume unit
#' @param volumetric_soil_moisture_unit character;
#' @param device character; device
#' @param location character; location
#' @param local_time character; local time
#' @param select character vector; columns to select, see [ar_schema()]
#'   for options of a table
#' @param start_time character; start formatted as "%Y-%m-%dT%H:%M:%SZ"
#' @param end_time character; end formatted as "%Y-%m-%dT%H:%M:%SZ"
#' @param verbose logical; verbose messages on API calls
#' @param recurse logical; should the function recurse until
#'   all data is fetched?
#' @rdname data
#' @export
#' @examples
#' \dontrun{
#' ar <- arable$new(username = Sys.getenv("AR_USERNAME"),
#'   password = Sys.getenv("AR_PASSWORD"))
#' ar_data(ar,
#'     table = "daily",
#'     limit = 50L,
#'     device = "C007538",
#'     select = c("time", "mean_rh"),
#'     start_time = format(Sys.time() - 60*60*24*30*12, "%Y-%m-%dT%H:%M:%SZ"),
#'     end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
#'     verbose = TRUE)
#' }
ar_data <- function(ar,
    table,
    cursor = NULL,
    limit = 1000L,
    order = c("asc", "desc"),
    temp = c("C", "F"),
    pres = c("mb", "kp"),
    ratio = c("dec", "pct"),
    size = c("in", "mm"),
    speed = c("mps", "kph", "mph"),
    vol = c("g", "l"),
    volumetric_soil_moisture_unit = c("millimeter_per_meter",
                                      "inches_per_foot"),
    device = NULL,
    location = NULL,
    local_time = NULL,
    select = NULL,
    start_time = NULL,
    end_time = NULL,
    verbose = FALSE,
    recurse = TRUE) {

  stopifnot(!missing(ar))
  stopifnot(inherits(ar, "arable"))
  stopifnot(!missing(table))
  stopifnot((!is.null(device) & is.null(location)) |
              (is.null(device) & !is.null(location)))
  stopifnot(is.integer(limit))
  stopifnot(1L <= limit & limit <= 10000L)

  order <- match.arg(order)
  temp <- match.arg(temp)
  pres <- match.arg(pres)
  ratio <- match.arg(ratio)
  size <- match.arg(size)
  speed <- match.arg(speed)
  vol <- match.arg(vol)
  volumetric_soil_moisture_unit <- match.arg(volumetric_soil_moisture_unit)

  if (!is.null(select))
    select <- paste(select, collapse = ",")


  qurl <- file.path(ar$base_url, "data", table)
  q <- list(
    cursor = cursor,
    limit = limit,
    order = order,
    temp = temp,
    pres = pres,
    ratio = ratio,
    size = size,
    speed = speed,
    vol = vol,
    volumetric_soil_moisture_unit = volumetric_soil_moisture_unit,
    device = device,
    location = location,
    select = select,
    start_time = start_time,
    end_time = end_time,
    local_time = local_time) %>%
    purrr::compact()

  if (verbose) {
    message("GET: ", httr::modify_url(qurl,
                                      query = q))
  }

  resp <- httr::GET(qurl,
                    query = q,
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

  out <- parsed

  if (recurse) {
    next_cursor <- resp$headers$`x-cursor-next`

    if (is.null(next_cursor)) {
      # base case (no next page)
      class(out) <- append("ar_data", class(out))
      return(out)
    }

    # recursive case (next page available)
    out <- c(
      out,
      ar_data(ar,
              table,
              cursor = next_cursor,
              limit = limit,
              order = order,
              temp = temp,
              pres = pres,
              ratio = ratio,
              size = size,
              speed = speed,
              vol = vol,
              volumetric_soil_moisture_unit = volumetric_soil_moisture_unit,
              device = device,
              location = location,
              local_time = local_time,
              select = select,
              start_time = start_time,
              end_time = end_time,
              verbose = verbose,
              recurse = recurse)
    )
  }

  class(out) <- append("ar_data", class(out))
  return(out)
}


#' parse data into a nice data.frame
#' @rdname data
#' @param x a data object, as returned by [ar_data()]
#' @export
ar_parse_data <- function(x) {
  stopifnot(inherits(x, "ar_data"))
  purrr::map_df(x, function(y) {
    y[is_empty(y)] <- NA
    data.frame(y)
    })
}
