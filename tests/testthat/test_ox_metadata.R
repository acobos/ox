context("ox_metadata")

# getting the file address
file <- system.file("extdata", "odm1.3_full_example_Optimal.xml",
                    package = "ox",
                    mustWork = TRUE)
# Parsing the xml file
doc <- XML::xmlParse(file)

# incorrect call
test_that("gives error when arg is not of expected class", {
  expect_error(ox_metadata(file))
})

# correct call
res <- ox_metadata(doc)

test_that("returns list object, with a list + 14 dataframes", {
  expect_is(res, "list")
  expect_true(length(res) == 16)
  # metadata elements
  expect_is(res$file_info, "list")
  expect_is(res$global_vars, "list")
  expect_is(res$event_def, "data.frame")
  expect_is(res$event_ref, "data.frame")
  expect_is(res$form_def, "data.frame")
  expect_is(res$form_ref, "data.frame")
  expect_is(res$group_def, "data.frame")
  expect_is(res$group_ref, "data.frame")
  expect_is(res$item_def, "data.frame")
  expect_is(res$item_ref, "data.frame")
  expect_is(res$codelist, "data.frame")
  expect_is(res$codelist_item, "data.frame")
  expect_is(res$codelist_ref, "data.frame")
  expect_is(res$units, "data.frame")
  expect_is(res$sites, "data.frame")
  expect_is(res$subjects, "data.frame")
})

# clean
rm(doc, file, res)

