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

# ------- Load data --------- #

test_that("the load.data function is able to load objects and files correctly",{
  x = rnorm(10)
  save(x,file ="save_data.RData")
  y = load.data("save_data.RData", "C:/Users/SERGIO/Documents/Github/quality_control")
  expect_equal("x",y)
})

