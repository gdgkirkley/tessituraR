
#' The TXN resource category
#'
#' @field apiClient The Tessitura API Client
#' @export
#'
TXN <- R6::R6Class(
  classname = "TXN",
  public = list(
    apiClient = NULL,
    initialize = function(apiClient = NULL) {
      if(!missing(apiClient)) {
        self$apiClient = apiClient
      }
    }
  )
)
