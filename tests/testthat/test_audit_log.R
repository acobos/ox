context("Audit log")

# getting the file address
file <- system.file("extdata", "odm1.3_full_example.xml",
                    package = "ox",
                    mustWork = TRUE)
# Parsing the xml file
doc <- XML::xmlParse(file)

# incorrect call
test_that("gives error when arg is not of expected class", {
  expect_error(ox_audit_log(file))
})

# correct call
res <- ox_audit_log(doc)

test_that("returns dataframe, at least 1 row, no factors", {
  expect_is(res, "data.frame")
  expect_false("factor" %in% unique(sapply(res, class)))
})

# no further tests, because if export is not odm1.3full,
# this dataframe will be empty

# clean
rm(doc, file, res)

