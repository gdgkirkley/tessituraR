library(httr)
library(jsonlite)
library(magrittr)

callTessi <- function(endpoint, credentials, request_type = "GET", method, data){

  url <- paste0(endpoint, method)

  result <- list()

  if(request_type == "GET"){

    result <- GET(
      url,
      authenticate(credentials["user"], credentials["pass"]),
      content_type_json()
      ) %>%
      content("text") %>%
      fromJSON(flatten=TRUE)

  } else if(request_type == "POST") {

    if(typeof(data) != "list"){stop()}
  # Continue working on the try catch aspect of this function
    tryCatch({
    result <- POST(
      url,
      authenticate(credentials["user"], credentials["pass"]),
      encode = "json",
      body=data
    )}, warning = function(war) {
        print(paste("Warning: ", war))
      }
    , ("Error retrieving data"))

  } else if(request_type == "PUT"){

  } else if(request_type == "DELETE"){

  }

  return(result)
}
