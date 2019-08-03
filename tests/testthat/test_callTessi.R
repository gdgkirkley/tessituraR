context('call the Tessitura API')

# Testing this function requires access to a Tessitura API

test_that('returns something', {
  result <- callTessi(tEndpoint, basePath, '/Diagnostics/Status', credentials)
  expect_that(result$Success, equals(TRUE))
})

test_that('returns a list', {
  result <- callTessi(tEndpoint, basePath, method, credentials)
  expect_type(result, 'list')
})
