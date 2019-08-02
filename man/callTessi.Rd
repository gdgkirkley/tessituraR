\name{callTessi}
\alias{callTessi}
\title{Send a Request to the Tessitura API}
\usage{
callTessi(host, basePath, method, credentials, request_type="GET", data, flatten=TRUE)
}
\description{
Send GET or POST requests to Tessitura. Function will return either a flattened dataframe or a list of the response. Credentials should be a base64 encoded string using createCredentials().
}
\examples{
callTessi()
}
