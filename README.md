
<!-- README.md is generated from README.Rmd. Please edit that file -->
tessituraR
==========

<!-- badges: start -->
<!-- badges: end -->
Interacting with and manipulating data from the Tessitura Service API. Inspired by Micah Walter's [tessitura-tools for Python](https://github.com/micahwalter/tessitura-tools).

Installation
------------

Hopefully this package will be up on CRAN soon. Until then you can install the development version with:

``` r
# install.packages("devtools")
devtools::install_github("gdgkirkley/tessituraR")
```

Example
-------

This is a basic example of usage:

``` r
library(tessituraR)

host <- "http://mytessi.tessituranetwork.com/"
basePath <- "TessituraService"
resource <- "/TXN/ModesofSale"

# Keep your credentials in environment variables!
credentials <- createCredentials("creif", "admin", "MET95", "impresario")

# Request is a GET by default. Change flatten to FALSE if you want the entire JSON object back.
modesOfSale <- callTessi(host, basePath, resource, credentials)
```
