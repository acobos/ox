context("Global variables")

# getting the file address
file <- system.file("extdata", "odm1.3_full_example_Optimal.xml",
                    package = "ox",
                    mustWork = TRUE)
# Parsing the xml file
library(XML)
doc <- xmlParse(file)

library(dplyr)
library(ox)

library(testthat)

# incorrect call
test_that("gives error when arg is not of expected class", {
  expect_error(ox_global_vars(file))
})

# correct call
res <- ox_global_vars(doc)

test_that("returns dataframe with expected variables, at least 1 row", {
  expect_is(res, "list")
  # vars
  expect_true("StudyName" %in% names(res))
  expect_true("StudyDescription" %in% names(res))
  expect_true("ProtocolName" %in% names(res))
})

# clean
rm(doc, file, res)
