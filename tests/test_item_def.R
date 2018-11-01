context("Item definitions")

# getting the file address
file <- system.file("extdata", "odm1.3_clinical_ext_example.xml",
                    package = "ox",
                    mustWork = TRUE)
# Parsing the xml file
library(XML)
doc <- xmlParse(file)

library(dplyr)
library(ox)

library(testthat)

# correct call
res <- ox_item_def(doc)

test_that("returns dataframe with expected variables, at least 1 row", {
  expect_is(res, "data.frame")
  # vars
  expect_true("study_oid" %in% names(res))
  expect_true("version" %in% names(res))
  expect_true("metadata_version" %in% names(res))
  expect_true("item_name" %in% names(res))
  expect_true("item_data_type" %in% names(res))
  expect_true("item_length" %in% names(res))
  expect_true("item_sas_field_name" %in% names(res))
  expect_true("item_comment" %in% names(res))
  expect_true("form_oid" %in% names(res))
  expect_true("item_significant_digits" %in% names(res))
  # rows
  expect_true(nrow(res) >= 1)
})

test_that("dataframe has no factors", {
  expect_false("factor" %in% unique(sapply(res, class)))
})

test_that("gives error when arg is not of expected class", {
  expect_error(ox_item_def(file))
})

# clean
rm(doc, file, res)

