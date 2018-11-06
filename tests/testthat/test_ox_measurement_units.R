context("ox_measurement_units()")

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
  expect_error(ox_measurement_units(file))
})

# correct call
res <- ox_measurement_units(doc)

test_that("returns dataframe", {
  expect_is(res, "data.frame")
})

# clean
rm(doc, file, res)

