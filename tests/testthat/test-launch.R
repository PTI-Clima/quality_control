dataOutFiles = getwd()
library(testthat)

# -------- year.new.dir.calc ------------ #

test_that("if year.new is NA, the function returns new_all", {
  year.new <- NA
  x <- year.new.dir.calc(year.new)
  expect_equal(x, "new_all")
})


test_that("if nchar(year.new) is <= 4, input = output", {
  year.new <- "2023"
  x <- year.new.dir.calc(year.new)
  expect_equal(x, year.new)
})


test_that("if nchar(year.new) is > 4, year.new.dir = new_all", {
  year.new <- "2023-04-19"
  x <- year.new.dir.calc(year.new)
  expect_equal(x, "new_all")
})

# ------ Prepare data ------- #

# test_that("If file in prepare.data does not exists, the function returns NA", {
#   dataOutFiles = "C:/Users/SERGIO/Documents"
#   
#   x <- prepare.data(C_HR, dataOutFiles)
#   expect_equal(x, NA)
# })

# ---------- Save Data Function ----------- #
test_that("the save.data function is able to save objects and files correctly",{
  set.seed(123)
  x = rnorm(10)
  save.data(x,file ="save_data.RData") 
  y = load("save_data.RData")
  expect_equal("x",y)
})


# ------- Load data --------- #

test_that("the load.data function is able to load objects and files correctly",{
  set.seed(123)
  x = rnorm(10)
  save(x,file ="save_data2.RData")
  y = load.data("save_data2.RData", dataOutFiles = dataOutFiles)
  expect_equal("x",y)
})

