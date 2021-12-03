

SeasonApi <- R6::R6Class(
  classname = "SeasonApi",
  public = list(
    apiClient = NULL,

    initialize = function(apiClient = NULL) {
      if(!is.null(apiClient)) {
        self$apiClient <- apiClient
      }
    },

    Get = function(id, ...) {
      apiResponse <- self$GetWithHTTP(id, ...)
      resp <- apiResponse$response
      if (httr::status_code(resp) >= 200 && httr::status_code(resp) <= 299) {
        apiResponse$content
      } else if (httr::status_code(resp) >= 300 && httr::status_code(resp) <= 399) {
        apiResponse
      } else if (httr::status_code(resp) >= 400 && httr::status_code(resp) <= 499) {
        apiResponse
      } else if (httr::status_code(resp) >= 500 && httr::status_code(resp) <= 599) {
        apiResponse
      }
    },

    GetWithHTTP = function(id, ...) {
      args <- list(...)
      queryParams <- list()
      headerParams <- c()

      if(missing(id)) {
        stop("Season ID is required")
      }

      urlPath <- paste0("/ReferenceData/Seasons/", id)

      resp <- self$apiClient$CallTessi(urlPath,
                                       method = "GET",
                                       queryParams = queryParams,
                                       headerParams = headerParams,
                                       body = NULL,
                                       ...)

      if (httr::status_code(resp) >= 200 && httr::status_code(resp) <= 299) {
        deserializedRespObj <- tryCatch(
          self$apiClient$deserialize(resp, "Season", loadNamespace("tessituraR")),
          error = function(e){
            stop("Failed to deserialize response")
          }
        )
        ApiResponse$new(deserializedRespObj, resp)
      }  else if (httr::status_code(resp) >= 300 && httr::status_code(resp) <= 399) {
        ApiResponse$new(paste("Server returned " , httr::status_code(resp) , " response status code."), resp)
      } else if (httr::status_code(resp) >= 400 && httr::status_code(resp) <= 499) {
        ApiResponse$new("API client error", resp)
      } else if (httr::status_code(resp) >= 500 && httr::status_code(resp) <= 599) {
        ApiResponse$new("API server error", resp)
      }
    }
  )
)
