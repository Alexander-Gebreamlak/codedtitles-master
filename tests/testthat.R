# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(here)
library(devtools)
devtools::install_github("drmomalik/codedtitles")
library(codedtitles)

data <- read.csv(here("data", "df.csv"), header = TRUE)
fake_numbers <- runif(20, min = 1, max = 100)  # Random numbers between 1 and 100

test_that("my coded title function works", {
  result <- codevar(data, max_length = 3, split = FALSE)
  new_data <- result

  # Compare the dataframes
  expect_equal(result, new_data)

  # Test that the function fails properly when a non-dataframe is passed
  expect_error(codevar(fake_numbers, max_length = 3), "Input data must be a dataframe")
})
