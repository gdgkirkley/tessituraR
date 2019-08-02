\name{createCredentials}
\alias{createCredentials}
\title{Creates Tessitura Service Credentials}
\usage{
createCredentials(username, usergroup, location, password)
}
\description{
Returns a base64 encoded string for interacting with the Tessitura Service API. It is recommended to store credentials in environment variables.
}
\examples{
createCredentials("creif", "admin", "MET95", "impresario")
}
