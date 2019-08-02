require(httr)
require(jsonlite)
require(magrittr)

callTessi <- function(host, basePath, method, credentials, request_type = "GET", data, flatten=TRUE){

  url <- paste0(host, basePath, method)

  result <- list()

  if(request_type == "GET"){

    result <- httr::GET(
      url,
      add_headers(.headers = c('Authorization' = credentials))
      )

  } else if(request_type == "POST") {

    if(typeof(data) != "list"){stop()}
  # Continue working on the try catch aspect of this function
    tryCatch({
    result <- POST(
      url,
      auth,
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

  if(status_code(result) != 200){
    stop(
      message("Your request returned status ", status_code(result)),
      message("For more information, visit http://en.wikipedia.org/wiki/Http_error_codes"),
      message("The message from the server was ", content(result))
      )
  }

  if(flatten == TRUE){
    return(
      result %>%
      content("text") %>%
      fromJSON(flatten=TRUE)
    )
  }

  return(result)
}

createCredentials <- function(username, usergroup, location, password) {
  cred <- paste0(
      "Basic ", jsonlite::base64_enc(
        paste(username, usergroup, location, password, sep=":")
      )
    )
  return(cred)
}

