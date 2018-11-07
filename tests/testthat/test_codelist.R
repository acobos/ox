context("Codelists")

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

# function ox_codelist ----

# incorrect call
test_that("gives error when arg is not of expected class", {
  expect_error(ox_codelist(file))
})

# correct call
res <- ox_codelist(doc)

test_that("returns dataframe with expected variables, at least 1 row", {
  expect_is(res, "data.frame")
  # vars
  expect_true("codelist_oid" %in% names(res))
  expect_true("codelist_name" %in% names(res))
  expect_true("codelist_data_type" %in% names(res))
  expect_true("sas_format_name" %in% names(res))
  # rows
  # a study can have no codelists
})

test_that("dataframe has no factors", {
  expect_false("factor" %in% unique(sapply(res, class)))
})

# cleaning
rm(res)

# function ox_codelist_item ----
# incorrect call
test_that("gives error when arg is not of expected class", {
  expect_error(ox_codelist_item(file))
})
# correct call
res <- ox_codelist_item(doc)

test_that("returns dataframe with expected variables, at least 1 row", {
  expect_is(res, "data.frame")
  # vars
  expect_true("codelist_oid" %in% names(res))
  expect_true("codelist_name" %in% names(res))
  expect_true("codelist_data_type" %in% names(res))
  expect_true("sas_format_name" %in% names(res))
  expect_true("coded_value" %in% names(res))
  expect_true("code_label" %in% names(res))
  # rows
  # a study can have no codelists
})

test_that("dataframe has no factors", {
  expect_false("factor" %in% unique(sapply(res, class)))
})

# cleaning
rm(res)


# function ox_codelist_ref ----

# incorrect call
test_that("gives error when arg is not of expected class", {
  expect_error(ox_codelist_ref(file))
})
# correct call
res <- ox_codelist_ref(doc)

test_that("returns dataframe with expected variables, at least 1 row", {
  expect_is(res, "data.frame")
  # vars
  expect_true("item_oid" %in% names(res))
  expect_true("codelist_oid" %in% names(res))
  # rows
  # a study can have no codelists
})

test_that("dataframe has no factors", {
  expect_false("factor" %in% unique(sapply(res, class)))
})

# clean
rm(doc, file, res)
