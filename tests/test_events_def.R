context("Event definitions")

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

vars <- c("study_oid", "version", "metadata_version",
          "event_oid", "event_name", "event_repeating", "event_type")

test_that("returns data frame with vars variables", {
  expect_is(ox_event_def(doc), "data.frame")
  expect_true("study_oid" %in% names(ox_event_def(doc)))
  expect_true("version" %in% names(ox_event_def(doc)))
  expect_true("metadata_version" %in% names(ox_event_def(doc)))
  expect_true("event_oid" %in% names(ox_event_def(doc)))
  expect_true("event_name" %in% names(ox_event_def(doc)))
  expect_true("event_repeating" %in% names(ox_event_def(doc)))
  expect_true("event_type" %in% names(ox_event_def(doc)))
  expect_true(nrow(ox_event_def(doc)) >= 1)
})

test_that("gives error when arg is not of expected class", {
  expect_error(ox_event_def(file))
})
