library(testthat)

# Test read_yml function with valid input
test_that("read_yml works with valid input", {
  # url <- "https://raw.githubusercontent.com/r-lib/usethis/master/extdata/desc-1.yml"
  # file <- system.file("extdata", "desc-2.yml", package = "usethis")
  url <- "https://raw.githubusercontent.com/phuse-org/phuse-scripts/master/development/R/scripts/Draw_Dist2_R.yml"
  file <- "/Volumes/HiMacData/GitHub/pkgs/rule-builder-app/inst/apps/cfg-01_rba.yaml"
  # res_url <- read_yml(url)
  # expect_equal(length(res_url), 4)
  # expect_equal(res_url$Package, "usethis")
  res_file <- read_yml(file)
  expect_equal(length(res_file), 4)
  expect_equal(res_file$Language$name, "R")
})

# Test read_yml function with invalid input
test_that("read_yml returns empty list with invalid input", {
  res_null <- read_yml(NULL)
  res_empty <- read_yml("")
  res_wrong_type <- read_yml(123)
  expect_equal(length(res_null), 0)
  expect_equal(length(res_empty), 0)
  expect_equal(length(res_wrong_type), 0)
})

test_that("read_yml returns error with invalid file path", {
  expect_error(read_yml("invalid_file.yml"), "File does not exist")
})

test_that("read_yml returns error with invalid URL", {
  expect_error(read_yml("http://invalid.url"), "URL does not exist")
})

test_that("read_yml returns error with invalid YAML format", {
  invalid_yaml <- "this is not valid YAML"
  expect_error(read_yml(textConnection(invalid_yaml)), "Invalid YAML format")
})

# Run tests
if (identical(as.character(as.environment(2)[["R_PACKAGE_NAME"]]), "")) {
  # If running as a script, run tests in this file
  test_dir(".")
} else {
  # If running as part of a package, run tests in the tests directory
  test_dir("tests")
}
