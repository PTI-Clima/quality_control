# ---------------------------------------------------------------------------- #

library(testthat)

# Define a context for the tests
context("Testing the qc.apply function")

# ---------------------------------------------------------------------------- #

test_that("the function generates the correct output file", {
  setwd("../../")
  init.variables()
  vars = "w"
  output.file <- "data_QC/metadata/w_ok.rds"
  exists <- file.exists(output.file)
  if (!exists){
    qc.apply(vars)
    exists <- TRUE
  }
  expect_true(exists)
})