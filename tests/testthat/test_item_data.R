context("Item data")

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
  expect_error(ox_item_data(file))
})

# correct call
res <- ox_item_data(doc)

test_that("returns dataframe with expected variables, at least 1 row", {
  expect_is(res, "data.frame")
  # vars
  expect_true("study_oid" %in% names(res))
  expect_true("metadata_version_oid" %in% names(res))
  expect_true("subject_key" %in% names(res))
  # expect_true("subject_id" %in% names(res))
  # expect_true("subject_status" %in% names(res))
  expect_true("event_oid" %in% names(res))
  expect_true("form_oid" %in% names(res))
  # expect_true("form_version" %in% names(res))
  # expect_true("form_status" %in% names(res))
  expect_true("group_oid" %in% names(res))
  expect_true("group_repeat_key" %in% names(res))
  expect_is(res$group_repeat_key, "numeric")
  expect_true("trasaction_type" %in% names(res))
  expect_true("item_oid" %in% names(res))
  expect_true("value" %in% names(res))

  # rows
  expect_true(nrow(res) >= 1)
})

test_that("dataframe has no factors", {
  expect_false("factor" %in% unique(sapply(res, class)))
})

# clean
rm(doc, file, res)

