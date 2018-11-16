context("Extract groups from ox object")

# getting the file address
file <- system.file("extdata", "odm1.3_full_example.xml",
                    package = "ox",
                    mustWork = TRUE)
# Parsing the xml file
doc <- XML::xmlParse(file)

# ox object
d <- ox_all(doc)

# correct call
res <- ox_xtract_group(d, "IG_DEMO_DEMOGRAPHICDATA")

# incorrect call
test_that("gives error when arg is not of expected class", {
  expect_error(ox_xtract_group(doc, "IG_ACUTE_UNGROUPED"))
  expect_error(ox_xtract_group(d))
  expect_error(ox_xtract_group(d, 1))
  expect_error(ox_xtract_group(d, "k"))
  expect_error(ox_xtract_group(d, TRUE, "k"))
})

test_that("returns dataframe with expected variables, at least 1 row", {
  expect_is(res, "data.frame")
  # vars
  expect_true("study_oid" %in% names(res))
  # expect_true("subject_id" %in% names(res))
  expect_true("subject_key" %in% names(res))
  expect_true("event_oid" %in% names(res))
  expect_true("form_oid" %in% names(res))
  expect_true("group_oid" %in% names(res))
  expect_true("group_repeat_key" %in% names(res))

  expect_is(res$study_oid, "character")
  # expect_is(res$subject_id, "character")
  expect_is(res$subject_key, "character")
  expect_is(res$event_oid, "character")
  expect_is(res$form_oid, "character")
  expect_is(res$group_oid, "character")
  expect_is(res$group_repeat_key, "numeric")
  expect_is(res$event_repeat_key, "numeric")
})

# clean
rm(doc, file, res, d)

