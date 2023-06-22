# ---------------------------------------------------------------------------- #

library(testthat)

# Define a context for the tests
# context("Testing the load.data function")

# ---------------------------------------------------------------------------- #

test_that("the load.data function is able to load objects and files correctly",{
  init.variables()
  set.seed(123)
  x <- rnorm(10)
  save(x,file = paste(dataOutFiles, sep = "", "/save_data2.RData"))
  y <- load.data("save_data2.RData")
  expect_equal("x",y)
})