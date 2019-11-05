require(httr)
require(jsonlite)
require(magrittr)
require(stringr)

#' Send a Request to the Tessitura API
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

