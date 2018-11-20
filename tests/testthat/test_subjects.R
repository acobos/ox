context("Subject data")

# getting the file address
file <- system.file("extdata", "odm1.3_full_example.xml",
                    package = "ox",
                    mustWork = TRUE)
# Parsing the xml file
doc <- XML::xmlParse(file)

# incorrect call
test_that("gives error when arg is not of expected class", {
  expect_error(ox_subjects(file))
})

# correct call
res <- ox_subjects(doc)

test_that("returns dataframe, at least 1 row", {
  expect_is(res, "data.frame")
  # vars
  expect_true("subject_key" %in% names(res) |
                "subject_id" %in% names(res))
  # rows
  expect_true(nrow(res) >= 1)
})

test_that("dataframe has no factors", {
  expect_false("factor" %in% unique(sapply(res, class)))
})

# clean
rm(doc, file, res)

