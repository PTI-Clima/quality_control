# ---------------------------------------------------------------------------- #

library(testthat)

# Define a context for the tests
# context("Testing the qc.apply function")

# ---------------------------------------------------------------------------- #

# test_that("the function generates the correct output file", {
#   if (basename(getwd()) != "quality_control") {
#     setwd("../../")
#   }
#   init.variables()
#   vars = c(C_W, C_HR, C_PR, C_IN, C_R)
#   for (v in vars) {
#     output.file <- paste0("data_QC/metadata/", v, "_ok.rds")
#     print(output.file)
#     exists <- file.exists(output.file)
#     if (!exists){
#       launch.all.controls()
#       exists <- TRUE
#     }
#     expect_true(exists)
#   }
# })