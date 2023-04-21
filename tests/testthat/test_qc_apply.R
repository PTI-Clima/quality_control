library(testthat)

# Define a context for the tests
context("Testing the qc.apply function")

test_that("the function throws an error when she does not recognize one of the variables", {
  init.variables()
  vars = "C_Prueba"
  expect_error(qc.apply(vars=vars, output.folder = "new_all", data.source = "AEMET"),
               "Error: The climatological variable is not valid")
})

test_that("the function is able to calculate tmax if var = t", {
  init.variables()
  vars = "t"
  qc.apply(vars = vars, 
           output.folder = "new_all", 
           data.source = "AEMET")
  x = load(
    paste(dataOutFiles, 
          sep = "", 
          "/tmax"))
  expect_s3_class(x, "data.frame")
})

test_that("the function is able to calculate tmin if var = t", {
  init.variables()
  vars = "t"
  qc.apply(vars = vars, output.folder = "new_all", data.source = "AEMET")
  x = load(paste(dataOutFiles, sep = "", "/tmin"))
  expect_s3_class(x, "data.frame")
})

