# ---------------------------------------------------------------------------- #

library(testthat)

# Define a context for the tests
context("Testing the prepare.data function")

# ---------------------------------------------------------------------------- #

test_that("Check that the function receives a valid climatological variable", {
  init.variables()
  var <- "foo"
  expect_error(prepare.data(var), paste0('Error: The following climatological variable is not valid: ', var))
})

test_that("Check that the function receives a non-null climatological variable", {
  init.variables()
  var <- NA
  expect_error(prepare.data(var), "The climatological variable cannot be null")
})

test_that("The function creates the correct output files", {
  init.variables()
  var <- "w"
  prepare.data(var)
  expect_true(file.exists("../..//data_QC/AEMET/data_coor/w.csv") & file.exists("../../data_QC/AEMET/data_sort/w.csv"))
})