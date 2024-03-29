% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/control.R
\name{formatTessiPostRequest}
\alias{formatTessiPostRequest}
\title{Format a Tessitura Post Request}
\usage{
formatTessiPostRequest(data, returnJSON = TRUE)
}
\arguments{
\item{data}{A data frame to transform}

\item{returnJSON}{Return data in JSON or list format. Default is JSON.}
}
\value{
A nested JSON string for POST requests to the Tessitura API
}
\description{
Since Tessitura requires nested JSON requests in most routes, this function
will transform a data frame containing columns with names separated by a point
into a nested list or JSON string. Each column should be named like "Keyword.Category.Id"
as the function will nest at each subsequent . character.
}
\examples{
data <- tribble(
    ~Keyword.Description, ~Keyword.Id, ~Keyword.Category.Id, ~Constituent.Id, ~Id, ~Value,
    "sample string 1", 2, 1, 1, 1, "sample string 3"
)
formattedRequest <- formatTessiPostRequest(data)

}
