context("Form definitions")

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
res <- ox_form_def(doc)

test_that("returns dataframe with expected variables, at least 1 row", {
  expect_is(res, "data.frame")
  # vars
  expect_true("study_oid" %in% names(ox_form_def(doc)))
  expect_true("version" %in% names(ox_form_def(doc)))
  expect_true("metadata_version" %in% names(ox_form_def(doc)))
  expect_true("form_oid" %in% names(ox_form_def(doc)))
  expect_true("form_name" %in% names(ox_form_def(doc)))
  expect_true("form_repeating" %in% names(ox_form_def(doc)))
  # rows
  expect_true(nrow(ox_form_def(doc)) >= 1)
})

test_that("dataframe has no factors", {
  expect_false("factor" %in% unique(sapply(res, class)))
})

test_that("gives error when arg is not of expected class", {
  expect_error(ox_form_def(file))
})

# clean
rm(doc, file, res)

