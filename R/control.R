require(httr)
require(jsonlite)
require(magrittr)

#' Send a Request to the Tessitura API
#'
#' @param host character
#' @param basePath character
#' @param method character
#' @param credentials character
#' @param request_type character
#' @param data list
#' @param flatten logical
#'
#' @return list
#' @export
#'
#' @examples
#'#' callTessi('mytessi.tessituranetwork.com/', 'TessituraService', '/Diagnostics/Status', 'mybase64credentials')

callTessi <- function(host, basePath, method, credentials, request_type = "GET", data, flatten=TRUE){

  url <- paste0(host, basePath, method)

  result <- list()

  if(request_type == "GET"){

    result <- httr::GET(
      url,
      httr::add_headers(.headers = c('Authorization' = credentials))
      )

  } else if(request_type == "POST") {

    if(typeof(data) != "list"){stop()}
  # Continue working on the try catch aspect of this function
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
      message("Your request returned status ", httr::status_code(result)),
      message("For more information, visit http://en.wikipedia.org/wiki/Http_error_codes"),
      message("The message from the server was ", httr::content(result))
      )
  }

  if(flatten == TRUE){
    return(
      result %>%
      httr::content("text") %>%
      jsonlite::fromJSON(flatten=TRUE)
    )
  }

  return(result)
}

#' Create Tessitura API Credentials
#'
#' @param username character
#' @param usergroup character
#' @param location character
#' @param password character
#'
#' @return character
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

