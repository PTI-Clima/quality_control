# ---------------------------------------------------------------------------- #

library(testthat)

# Define a context for the tests
context("Testing the year.new.dir.calc function")

# ---------------------------------------------------------------------------- #

test_that("if year.new is NA, the function returns data_QC", {
  year.new <- NA
  x <- year.new.dir.calc(year.new)
  expect_equal(x, "data_QC")
})

# ---------------------------------------------------------------------------- #

test_that("if nchar(year.new) is <= 4, input = output", {
  year.new <- "2023"
  x <- year.new.dir.calc(year.new)
  expect_equal(x, year.new)
})

# ---------------------------------------------------------------------------- #

test_that("if nchar(year.new) is > 4, year.new.dir = data_QC", {
  year.new <- "2023-04-19"
  x <- year.new.dir.calc(year.new)
  expect_equal(x, "data_QC")
})

# ---------------------------------------------------------------------------- #