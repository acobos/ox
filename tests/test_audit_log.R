context("Audit log")

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
  expect_error(ox_audit_log(file))
})

# correct call
res <- ox_audit_log(doc)

test_that("returns dataframe, at least 1 row", {
  expect_is(res, "data.frame")

  # no further tests on vars or rows, because if data export does not contain
  # audit log entries, a void dataframe is returned (0 vars, 0 rows)
})

test_that("dataframe has no factors", {
  expect_false("factor" %in% unique(sapply(res, class)))
})

# clean
rm(doc, file, res)

