\name{callTessi}
\alias{callTessi}
\title{Send a Request to Tessitura}
\usage{
callTessi(endpoint, credentials, request_type="GET", method, data, flatten=TRUE)
}
\description{
Send GET or POST requests to Tessitura. Function will return either a flattened dataframe or a list of the response. Credentials should be supplied as a list object with user and password elements. It is recommended to pass these via environment variables.
}
\examples{
callTessi()
}
