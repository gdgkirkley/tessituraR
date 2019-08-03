require(httr)
require(jsonlite)
require(magrittr)

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
#'#' callTessi('mytessi.tessituranetwork.com/', 'TessituraService', '/Diagnostics/Status', 'mybase64credentials')

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
           extract
           )
        )
    }
  }

  return(result)
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

