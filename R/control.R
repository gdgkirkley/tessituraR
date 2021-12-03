require(httr)
require(jsonlite)
require(magrittr)
require(stringr)

#' A TessituraService class
#'
#' This is a more useable Tessitura Service class that can be implemented by
#' further objects to create requests.
#'
#' @docType class
#' @title TessituraService
#' @field tessituraUrl Base Tessitura REST API url
#' @field credentials Base64 encoded credentials - can be created with createCredentials()
#' @field defaultHeaders Default headers for all requests
#' @field timeout The maximum timeout for a request
#' @field maxRetryAttempts The maximum number of times to retry a request before failing
#' @export
#'
#' @examples
#' TessituraService$new(
#' tessituraUrl = "https://mytessi.tessituranetwork.com",
#' credentials = createCredentials(username="creif", usergroup="admin", location="MET95", password="impresario")
#' )
TessituraService <- R6::R6Class(
  classname = "TessituraService",
  public = list(
    tessituraUrl = NULL,
    credentials = NULL,
    defaultHeaders = NULL,
    timeout = NULL,
    maxRetryAttempts = NULL,
    userAgent = "TessituraUser/1.0.0/r",

    #' @description Constructor
    #'
    #' @param tessituraUrl Base Tessitura REST API url
    #' @param credentials Base64 encoded credentials
    #' @param defaultHeaders Default headers for all requests
    #' @param timeout The maximum timeout for a request. Defaults to 5000.
    #' @param maxRetryAttempts The maximum number of times to retry a request before failing. Defaults to 3.
    #' @param userAgent The user agent to use in requests
    #'
    initialize = function(tessituraUrl, credentials, defaultHeaders = NULL, userAgent = NULL, timeout = 5000, maxRetryAttempts = 3) {
      if(!is.null(tessituraUrl)) {
        self$tessituraUrl <- tessituraUrl
      }

      if(!is.null(credentials)) {
        self$credentials <- credentials
      }

      if(!is.null(defaultHeaders)) {
        self$defaultHeaders <- defaultHeaders
      }

      if(!is.null(userAgent)) {
        self$userAgent <- userAgent
      }

      if (!is.null(timeout)) {
        self$timeout <- timeout
      }

      if(!is.null(maxRetryAttempts)) {
        self$maxRetryAttempts <- maxRetryAttempts
      }
    },

    #'
    #' @description Call a Tessitura endpoint with retries
    #'
    #' @param url The endpoint to request relative to the tessituraUrl
    #' @param method The HTTP method
    #' @param queryParams The query parameters for the request
    #' @param headerParams Any additional headers to attach to the request
    #' @param body The request body for POST and PUT methods
    #' @param ... Any other parameters for the httr methods
    #'
    CallTessi = function(url, method, queryParams = list(), headerParams = c(), body = NULL, ...) {
      retryStatusCodes <- c(500)
      resp <- self$Execute(url, method, queryParams, headerParams, body, ...)
      statusCode <- httr::status_code(resp)

      if (is.null(self$maxRetryAttempts)) {
        self$maxRetryAttempts = 3
      }

      for (i in 1 : self$maxRetryAttempts) {
        if (statusCode %in% retryStatusCodes) {
          Sys.sleep((2 ^ i) + stats::runif(n = 1, min = 0, max = 1))
          resp <- self$Execute(url, method, queryParams, headerParams, body, ...)
          statusCode <- httr::status_code(resp)
        } else {
          break
        }
      }

      resp
    },

    #'
    #' @description Execute a request
    #'
    #' @param url The endpoint to request relative to the tessituraUrl
    #' @param method The HTTP method
    #' @param queryParams The query parameters for the request
    #' @param headerParams Any additional headers to attach to the request
    #' @param body The request body for POST and PUT methods
    #' @param ... Any other parameters for the httr methods
    #'
    Execute = function(url, method, queryParams, headerParams, body, ...) {
      headers = httr::add_headers(
        c(headerParams, self$defaultHeaders, Authorization = self$credentials)
        )

      path = paste0(self$tessituraUrl, url)

      if (method == "GET") {
        httr::GET(path, query = queryParams, headers, httr::timeout(self$timeout),
                  httr::user_agent(self$`userAgent`), ...)
      } else if (method == "POST") {
        httr::POST(path, query = queryParams, headers, body = body,
                   httr::content_type("application/json"), httr::timeout(self$timeout),
                   httr::user_agent(self$`userAgent`), ....)
      } else if (method == "PUT") {
        httr::PUT(path, query = queryParams, headers, body = body,
                   httr::content_type("application/json"), httr::timeout(self$timeout),
                  httr::user_agent(self$`userAgent`), ....)
      } else if (method == "DELETE") {
        httr::DELETE(path, query = queryParams, headers, httr::timeout(self$timeout),
                     httr::user_agent(self$`userAgent`), ...)
      } else {
        error <- "HTTP Method must be GET, POST, PUT or DELETE"
        stop(error)
      }
    },

    #' @description Deserialize the content of api response to the given type.
    #'
    #' @param resp The response object
    #' @param returnType The type of object to return
    #' @param pkgEnv The environment to find the type in
    #'
    deserialize = function(resp, returnType, pkgEnv) {
      respObj <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
      self$deserializeObj(respObj, returnType, pkgEnv)
    },

    #' @description Deserialize the response from jsonlite object based on the given type
    #' by handling complex and nested types by iterating recursively
    #' Example returnTypes will be like "array[integer]", "map(Performance)", "array[map(Performance)]", etc.,
    #'
    #' @param obj The object to deserialize
    #' @param returnType The type of object to return
    #' @param pkgEnv The environment to find the type in
    #'
    deserializeObj = function(obj, returnType, pkgEnv) {
      returnObj <- NULL
      primitiveTypes <- c("character", "numeric", "integer", "logical", "complex")

      if (startsWith(returnType, "map(")) {
        innerReturnType <- regmatches(returnType, regexec(pattern = "map\\((.*)\\)", returnType))[[1]][2]
        returnObj <- lapply(names(obj), function(name) {
          self$deserializeObj(obj[[name]], innerReturnType, pkgEnv)
        })
        names(returnObj) <- names(obj)
      }

      else if (startsWith(returnType, "array[")) {
        innerReturnType <- regmatches(returnType, regexec(pattern = "array\\[(.*)\\]", returnType))[[1]][2]
        if (c(innerReturnType) %in% primitiveTypes) {
          returnObj <- vector("list", length = length(obj))
          if (length(obj) > 0) {
            for (row in 1:length(obj)) {
              returnObj[[row]] <- self$deserializeObj(obj[row], innerReturnType, pkgEnv)
            }
          }
        } else {
          if(!is.null(nrow(obj))){
            returnObj <- vector("list", length = nrow(obj))
            if (nrow(obj) > 0) {
              for (row in 1:nrow(obj)) {
                returnObj[[row]] <- self$deserializeObj(obj[row, , drop = FALSE], innerReturnType, pkgEnv)
              }
            }
          }
        }
      }

      else if (exists(returnType, pkgEnv) && !(c(returnType) %in% primitiveTypes)) {
        returnType <- get(returnType, envir = as.environment(pkgEnv))
        returnObj <- returnType$new()
        returnObj$fromJSON(
          jsonlite::toJSON(obj, digits = NA, auto_unbox = TRUE)
        )
      }

      else {
        returnObj <- obj
      }
      returnObj
    },
    #' @description Flatten and unwrap a response to a data frame
    #'
    #' @param response The HTTP response object
    #' @returns A flattened result data frame
    #'
    flattenResponse = function(response) {
      flatResult <- response %>%
        httr::content("text") %>%
        jsonlite::fromJSON(flatten=TRUE)

      if(inherits(flatResult, "data.frame")){
        return(flatResult)
      } else {
        return(
          purrr::map_dfr(
            unlist(flatResult),
            magrittr::extract
          )
        )
      }
    }
  )
)

#' Create a new Tessitura Service object
#' @param tessituraUrl The base url for your Tessitura REST API
#' @param credentials A base64 encded character string. Can be created with createCredentials()
#'
#' @return a TessituraService S4 object
#' @export
#'
createTessituraService = function(tessituraUrl, credentials) {
  return(
    TessituraService$new(tessituraUrl, credentials)
  )
}

#' Send a Request to the Tessitura API - Deprecated
#'
#' @param host The host name for your Tessitura API
#' @param basePath The base path for your Tessitura API. eg., TessituraService
#' @param resource The resource to be request from the API. eg., Diagnostics/Status
#' @param credentials A base64 encoded character string. Can be created with createCredentials()
#' @param request_type An http verb. Currently on GET and POST are supported
#' @param data The data object for POST.
#' @param flatten If true, a data frame will be returned of the results. If false, the result object will be returned.
#'
#' @return A data frame or list of the result
#' @export
#'
#' @examples
#'host <- 'mytessi.tessituranetwork.com/'
#'basePath <- 'TessituraService'
#'resource <- '/Diagnostics/Status'
#'credentials <- 'mybase64credentials'
#'#' callTessi(host, basePath, resource, credentials)

callTessi <- function(host, basePath, resource, credentials, request_type = "GET", data, flatten=TRUE){

  url <- paste0(host, basePath, resource)

  result <- list()

  if(request_type == "GET"){

    result <- httr::GET(
      url,
      httr::add_headers(.headers = c('Authorization' = credentials))
      )

  } else if(request_type == "POST") {

    if(typeof(data) != "list"){stop("Data must be a data frame")}

    topLevelNames <- list()
    for(column in data){
      if(stringr::str_detect(names(data), ".")){

      }
    }

    tryCatch({
    result <- httr::POST(
      url,
      httr::add_headers(.headers = c('Authorization' = credentials)),
      encode = "json",
      body=data
    )}, warning = function(war) {
        print(paste("Warning: ", war))
    }, error = function(err) {
        print(paste("Error: ", err))
      }
    )

  } else if(request_type %in% c("PUT", "DELETE")){
    stop("This type of request is not yet supported.")
  } else {
    stop("There was a problem with your request type.")
  }

  if(httr::status_code(result) != 200){
    stop(
     writeLines(
       paste0("Your request returned status ", httr::status_code(result), "\n",
      "For more information, visit http://en.wikipedia.org/wiki/Http_error_codes\n",
      "The message from the server was '", strtrim(httr::content(result), 100), "'"
          )
        )
      )
  }

  if(flatten == TRUE){

    flatResult <- result %>%
      httr::content("text") %>%
      jsonlite::fromJSON(flatten=TRUE)

    if(inherits(flatResult, "data.frame")){
      return(flatResult)
    } else {
      return(
         purrr::map_dfr(
           unlist(flatResult),
           magrittr::extract
           )
        )
    }
  }

  return(result)
}


#' @title  Format a Tessitura Post Request
#'
#' @description Since Tessitura requires nested JSON requests in most routes, this function
#' will transform a data frame containing columns with names separated by a point
#' into a nested list or JSON string. Each column should be named like "Keyword.Category.Id"
#' as the function will nest at each subsequent . character.
#'
#' @param data A data frame to transform
#' @param returnJSON Return data in JSON or list format. Default is JSON.
#'
#' @return A nested JSON string for POST requests to the Tessitura API
#' @export
#'
#' @examples
#' data <- tribble(
#'     ~Keyword.Description, ~Keyword.Id, ~Keyword.Category.Id, ~Constituent.Id, ~Id, ~Value,
#'     "sample string 1", 2, 1, 1, 1, "sample string 3"
#' )
#' formattedRequest <- formatTessiPostRequest(data)
#'
formatTessiPostRequest <- function(data, returnJSON = TRUE) {

  splitColumnNames <- strsplit(names(data), "[.]")

  nestedData <- list()

  # This should be updated to work recursively for each level
  for(name in names(data)){

    splitColumnName <- unlist(strsplit(name, "[.]"))

    if(length(splitColumnName) > 1){

      for(level in splitColumnName){

        innerList <- list()

        if((which(level == splitColumnName) == 1))
        {
            next
        }
        else
        {
          innerList[[level]] <- data[[name]]
          nestedData[[splitColumnName[1]]] <- innerList
        }
      }
    }
    else
    {
      nestedData[[name]] <- data2[[name]]
    }
  }

  if(returnJSON){
    return(jsonlite::toJSON(nestedData, pretty=TRUE))
  } else {
    return(nestedData)
  }
}

#' Create Tessitura API Credentials
#'
#' @param username Your Tessitura user name
#' @param usergroup Your Tessitura user group
#' @param location Your Tessitura machine location
#' @param password Your Tessitura password
#'
#' @return A base64 encoded character string for authorization
#' @export
#'
#' @examples
#' #' createCredentials(username="creif", usergroup="admin", location="MET95", password="impresario")
createCredentials <- function(username, usergroup, location, password) {
  cred <- paste0(
      "Basic ", jsonlite::base64_enc(
        paste(username, usergroup, location, password, sep=":")
      )
    )
  return(cred)
}

