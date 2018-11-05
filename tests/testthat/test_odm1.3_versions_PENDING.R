context("odm1.3_clinical_ext")

# getting the file address
# file <- system.file("extdata", "odm1.3_clinical_ext_example.xml",
#                     package = "ox",
#                     mustWork = TRUE)

dir <- "C:/Users/Administrador/Documents/My Dropbox/Common/SOPs/FSS/R/DM OpenClinica/ox testing"
file <- paste(dir,
              "odm1.3_clinical_ext_Prueba_todos_2018-11-05-124602172.xml",
              sep="/")


# Parsing the xml file
library(XML)
doc <- xmlParse(file)

library(dplyr)
library(ox)

# ox object
d <- ox(doc)

library(testthat)

# incorrect call

test_that("returns dataframe with expected variables, at least 1 row", {
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

# clean
rm(doc, file, res)

