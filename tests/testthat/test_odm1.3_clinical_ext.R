context("odm1.3_clinical_ext")

# getting the file address
file <- system.file("extdata", "odm1.3_clinical_ext_example_Optimal.xml",
                    package = "ox",
                    mustWork = TRUE)

# Parsing the xml file
library(XML)
doc <- xmlParse(file)

library(dplyr)
library(ox)

# ox object
d <- ox_all(doc)

library(testthat)

test_that("ox object has all data and metadata dataframes", {
  expect_is(d$data, "data.frame")
  expect_is(d$metadata$event_def, "data.frame")
  expect_is(d$metadata$event_ref, "data.frame")
  expect_is(d$metadata$form_def, "data.frame")
  expect_is(d$metadata$form_ref, "data.frame")
  expect_is(d$metadata$group_def, "data.frame")
  expect_is(d$metadata$group_ref, "data.frame")
  expect_is(d$metadata$item_def, "data.frame")
  expect_is(d$metadata$item_ref, "data.frame")
  expect_is(d$metadata$codelist, "data.frame")
  expect_is(d$metadata$codelist_item, "data.frame")
  expect_is(d$metadata$codelist_ref, "data.frame")
  expect_is(d$metadata$units, "data.frame")
  expect_is(d$metadata$sites, "data.frame")
  expect_is(d$metadata$subjects, "data.frame")
})

test_that("ox_xtract_group() works", {
  expect_is(ox_xtract_group(d, "IG_DEMO_DEMOGRAPHICDATA"), "data.frame")
  expect_is(ox_xtract_group(d, "IG_DEMO_DEMOGRAPHICDATA",
                            use_item_names = TRUE,
                            define_factors = TRUE), "data.frame")
})

test_that("Extra variables in item_data are kept", {
  expect_true("subject_id" %in% names(d$data))
  expect_true("subject_status" %in% names(d$data))
  expect_true("form_version" %in% names(d$data))
  expect_true("form_status" %in% names(d$data))
})

test_that("ox_audit_log() gives message", {
  expect_true("subject_id" %in% names(d$data))
  expect_true("subject_status" %in% names(d$data))
  expect_true("form_version" %in% names(d$data))
  expect_true("form_status" %in% names(d$data))
  expect_message(ox_audit_log(doc))
  expect_null(ox_audit_log(doc))
})

# clean
rm(doc, file, d)

