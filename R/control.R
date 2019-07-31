require(httr)
require(jsonlite)
require(magrittr)

callTessi <- function(endpoint, credentials, request_type = "GET", method, data, flatten=TRUE){

  url <- paste0(endpoint, method)

  result <- list()

  auth <- if(typeof(credentials) == "list") {
    authenticate(credentials["user"], credentials["pass"])
  } else {
    NULL
  }

  if(request_type == "GET"){

    result <- httr::GET(
      url,
      auth,
      content_type_json()
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
    return(print("This type of request is not yet supported."))
  } else {
    stop(print("There was a problem with your request type."))
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
