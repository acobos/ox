context("Event references")

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
  expect_error(ox_event_ref(file))
  expect_error(ox_event_ref(doc, "kk"))
  expect_error(ox_event_ref(doc, c(TRUE, TRUE)))
})

# correct call
res <- ox_event_ref(doc)
res_simple <- ox_event_ref(doc, simplify = TRUE)


test_that("returns dataframe with expected variables, at least 1 row", {
  expect_is(res, "data.frame")
  # vars
  expect_true("study_oid" %in% names(res))
  expect_true("version" %in% names(res))
  expect_true("metadata_version" %in% names(res))
  expect_true("event_oid" %in% names(res))
  expect_true("event_order" %in% names(res))
  expect_true("event_mandatory" %in% names(res))
  # rows
  expect_true(nrow(res) >= 1)
})

test_that("dataframe has no factors", {
  expect_false("factor" %in% unique(sapply(res, class)))
})

# correct call with simplify = TRUE
test_that("returns dataframe with expected variables, at least 1 row", {
  expect_is(res_simple, "data.frame")
  # vars
  expect_true("event_oid" %in% names(res_simple))
  expect_true("event_order" %in% names(res_simple))
  expect_true("event_mandatory" %in% names(res_simple))
  expect_true(length(res_simple) == 3)
  # rows
  expect_true(nrow(res_simple) >= 1)
  # no factors
  expect_false("factor" %in% unique(sapply(res_simple, class)))
})

# clean
rm(doc, file, res)
