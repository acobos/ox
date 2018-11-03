context("ox()")

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

# incorrect call
test_that("gives error when arg is not of expected class", {
  expect_error(ox(file))
})

# correct call
res <- ox(doc)

test_that("returns dataframe with expected variables, at least 1 row", {
  expect_is(res, "ox")
  # ox list elements
  expect_true("data" %in% names(res))
  expect_true("metadata" %in% names(res))
  # ox data
  expect_is(res$data, "data.frame")
  # ox metadata
  expect_is(res$metadata$global_vars, "list")
  expect_is(res$metadata$event_def, "data.frame")
  expect_is(res$metadata$event_ref, "data.frame")
  expect_is(res$metadata$form_def, "data.frame")
  expect_is(res$metadata$form_ref, "data.frame")
  expect_is(res$metadata$group_def, "data.frame")
  expect_is(res$metadata$group_ref, "data.frame")
  expect_is(res$metadata$item_def, "data.frame")
  expect_is(res$metadata$item_ref, "data.frame")
  expect_is(res$metadata$codelist, "data.frame")
  expect_is(res$metadata$codelist_item, "data.frame")
  expect_is(res$metadata$codelist_ref, "data.frame")
  expect_is(res$metadata$units, "data.frame")
  expect_is(res$metadata$sites, "data.frame")
  expect_is(res$metadata$subjects, "data.frame")
})

# clean
rm(doc, file, res)

