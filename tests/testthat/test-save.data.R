# ---------------------------------------------------------------------------- #

library(testthat)

# Define a context for the tests
context("Testing the save.data function")

# ---------------------------------------------------------------------------- #

test_that("the save.data function is able to save objects and files correctly",{
  init.variables("data_QC")
  set.seed(123)
  x <- rnorm(10)
  save.data(x,file ="save_data.RData")
  y <- load(paste(dataOutFiles, sep = "", "/save_data.RData"))
  expect_equal("x",y)
})