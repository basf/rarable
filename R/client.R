#' @title arable API client
#' @description handles the authentication scheme and provides a basic ping method.
#'
#' @export
#' @return An object of class `arable`.
#' @examples
#' \dontrun{
#' ar <- arable$new(username = Sys.getenv("AR_USERNAME"),
#'   password = Sys.getenv("AR_PASSWORD"))
#' ar
#' ar$get_auth()
#' ar$ping()
#'
#' arable$new(apikey = Sys.getenv("AR_APIKEY"))
#' }
arable <- R6::R6Class("arable",
  public = list(
    #' @field base_url root url to api
    base_url = "https://api.arable.cloud/api/v2",

    #' @description print method for `client`
    #' @param x self
    #' @param ... ignored
    print = function(...) {
      cat("  url: ", self$base_url, "\n", sep = "")

      con <- ifelse(is.null(private$auth$headers),
                    FALSE,
                    paste0(substr(private$auth$headers, 0, 15), "..."))

      cat("  Authentication:  ", con, "\n", sep = "")

      invisible(self)
    },


    #' @description Create a new client object using different methods.
    #' The Arable API provides three methods of authentication to the API:
    #' username + password (basic auth), apikey or bearer token.
    #' Precedance is token > apikey > username + password
    #' @param username username
    #' @param password password
    #' @param apikey apikey
    #' @param token token
    initialize = function(username,
        password,
        apikey,
        token) {
      if (!missing(token)) {
        private$token_auth(token)
        return(self)
      }
      if (!missing(apikey)) {
        private$apikey_auth(apikey)
        return(self)
      }
      if (!missing(username) & !missing(password)) {
        private$basic_auth(username, password)
        return(self)
      }
      stop("No credentials supplied!")
    },

    #' @description Get authentication header
    #' @return a httr request object
    get_auth = function() private$auth,

    #' @description Ping API by calling /devices endpoint.
    #' Returns TRUE if we get a 200
    ping = function() {
      resp <- httr::GET(file.path(self$base_url, "devices"),
                                query = list(page = 1L, limit = 1),
                                private$auth)
      return(httr::status_code(resp) == 200)
    }
  ),

  private = list(
    auth = NULL,

    apikey_auth = function(apikey) {
      private$auth <- httr::add_headers(
        Authorization = paste0("Apikey ", apikey))
    },

    token_auth = function(apikey) {
      private$auth <- httr::add_headers(
        Authorization = paste0("Bearer ", token))
    },

    basic_auth = function(username, password) {
      cred <- paste0(username, ":", password) %>%
        jsonlite::base64_enc()
      private$auth <- httr::add_headers(Authorization = paste0("Basic ", cred))
    }
  )
)
