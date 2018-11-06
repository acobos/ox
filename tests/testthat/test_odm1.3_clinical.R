context("odm1.3_clinical_ext")

# getting the file address
file <- system.file("extdata", "odm1.3_clinical_example_Optimal.xml",
                    package = "ox",
                    mustWork = TRUE)

# Parsing the xml file
library(XML)
doc <- xmlParse(file)

library(ox)

library(testthat)

test_that("if file is odm1.3_clinical, then sorry message", {
  expect_message(ox_odm1.3_type(doc))
})


# clean
rm(doc, file, res)

