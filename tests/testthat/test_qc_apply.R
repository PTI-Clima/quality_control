library(testthat)

# Define a context for the tests
context("Testing the qc.apply function")

test_that("the function throws an error when she does not recognize one of the variables", {
  init.variables()
  vars = "C_Prueba"
  expect_error(qc.apply(vars=vars,input.folder = "data", output.folder = "new_all", data.source = "AEMET"),
               "Error: The climatological variable is not valid")
})
