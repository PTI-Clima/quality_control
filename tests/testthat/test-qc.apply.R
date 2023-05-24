# ---------------------------------------------------------------------------- #

library(testthat)

# Define a context for the tests
# context("Testing the qc.apply function")

# ---------------------------------------------------------------------------- #

test_that("the function throws an error when it does not recognize one of the variables", {
  init.variables()
  vars = "C_Prueba"
  expect_error(qc.apply(vars=vars), paste0('Error: The following climatological variable is not valid: ', vars))
})

test_that("the function throws an error when the climatalogical variables are a null vector", {
  init.variables()
  vars = NA
  expect_error(qc.apply(vars=vars), "The climatological variable cannot be null")
})

test_that("the function generates the correct output file", {
  print(getwd())
  print(basename(getwd()))
  setwd("../../../quality_control")
  init.variables()
  vars = "r"
  output.file <- "data_QC/metadata/r_ok.rds"
  exists <- file.exists(output.file)
  if (!exists){
    qc.apply(vars)
    exists <- TRUE
  }
  expect_true(exists)
})