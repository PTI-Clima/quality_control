library(testthat)

# Define a context for the tests
# context("Testing the correctUnits function")

# Create the object series with the aim of testing the function
set.seed(12)
series_w <- sample(1:99, 10, replace = T)
series_t <- sample(50:336, 10, replace = T)

test_that("the function throws an error when it does not recognize one of the variables", {
  init.variables()
  type = "C_Prueba"
  expect_error(correctUnits(series_w, type),
               "Error: The Climatological variable is not valid")
})

test_that("if the input variable is the insolation or the precipitation, 
          the function is doing exactly what it has been created for", {
  init.variables()
  type = "in"
  a = series_w/10
  expect_equal(a, correctUnits(series_w, "in"))
})

test_that("if the input variable is the wind speed, the function is doing exactly
          what it has been created for", {
            init.variables()
            type = "w"
            a = 0.75 * ((series_w * 1000) / 3600)
            expect_equal(a, correctUnits(series_w, "w"))
          })

test_that("if the input variable is the maximum or the minimum temperature, 
          the function is doing exactly what it has been created for", {
            init.variables()
            type = "tmax"
            a = series_t/10
            expect_equal(a, correctUnits(series_t, "tmax"))
})

test_that("if the input variable is the relative humidity, the function 
          does nothing", {
            init.variables()
            type = "hr"
            a = series_w
            expect_equal(a, correctUnits(series_w, "hr"))
          })

test_that("if the input variable is the radiation, the function 
          does nothing", {
            init.variables()
            type = "r"
            a = series_w
            expect_equal(a, correctUnits(series_w, "r"))
          })

test_that("the function returns an object of type double", {
  init.variables()
  type = "w"
  a = correctUnits(series_t, "w")
  expect_type(a, "double")
})

test_that("the function returns NA when the series is NA", {
  init.variables()
  series = NA
  type = "w"
  a = correctUnits(series, "w")
  expect_equal(a, NA)
})
