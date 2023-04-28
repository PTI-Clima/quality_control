# ---------------------------------------------------------------------------- #

library(testthat)

# Define a context for the tests
context("Testing the qc.apply function")

# ---------------------------------------------------------------------------- #

test_that("All defined variables exists", {
  init.variables()
  var.list <- c("C_AEMET", "dataOutFiles", "dataFiles", 
                "C_W", "C_HR", "C_PR", "C_IN", "C_R", "C_T")
  expect_true(all(sapply(var.list, exists)))
})
