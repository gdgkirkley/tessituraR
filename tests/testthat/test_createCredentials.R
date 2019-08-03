context('Create a base64 credential string')

test_that("returns a character string", {
  result <- createCredentials("creif", "admin", "MET95", "impresario")
  expect_type(result, "character")
})

test_that("contains Basic string", {
  result <- createCredentials("creif", "admin", "MET95", "impresario")
  expect_match(result, regexp = "Basic")
})

test_that("is base64 encoded", {
  result <- createCredentials("creif", "admin", "MET95", "impresario")
  result <- stringr::str_remove(result, "Basic ")
  expect_match(result, regexp="^([A-Za-z0-9+/]{4})*([A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{2}==)?$")
})
