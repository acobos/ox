context("Event definitions")

# getting the file address
file <- system.file("extdata", "odm1.3_full_example.xml",
                    package = "ox",
                    mustWork = TRUE)
# Parsing the xml file
doc <- XML::xmlParse(file)

# incorrect call
test_that("gives error when arg is not of expected class", {
  expect_error(ox_event_def(file))
})

# correct call
res <- ox_event_def(doc)

test_that("returns dataframe with expected variables, at least 1 row", {
  expect_is(res, "data.frame")
  # vars
  expect_true("event_oid" %in% names(res))
  expect_true("event_name" %in% names(res))
  expect_true("event_repeating" %in% names(res))
  expect_true("event_type" %in% names(res))
  # rows
  expect_true(nrow(res) >= 1)
})

test_that("dataframe has no factors", {
  expect_false("factor" %in% unique(sapply(res, class)))
})

# clean
rm(doc, file, res)
