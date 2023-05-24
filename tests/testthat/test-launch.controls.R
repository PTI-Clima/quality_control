# ---------------------------------------------------------------------------- #

library(testthat)

# Define a context for the tests
# context("Testing the qc.apply function")

# ---------------------------------------------------------------------------- #

test_that("the function generates the correct output file", {
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