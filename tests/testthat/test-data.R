test_that("sample_corpus data object exists and has correct structure", {
  data("sample_corpus", package = "quanteda.extras")
  
  expect_s3_class(sample_corpus, "data.frame")
  expect_true("doc_id" %in% names(sample_corpus))
  expect_true("text" %in% names(sample_corpus))
  expect_equal(ncol(sample_corpus), 2)
  expect_true(nrow(sample_corpus) > 0)
  expect_type(sample_corpus$doc_id, "character")
  expect_type(sample_corpus$text, "character")
})

test_that("ds_dict data object exists and has correct structure", {
  data("ds_dict", package = "quanteda.extras")
  
  expect_true(inherits(ds_dict, "dictionary2"))
  expect_true(length(ds_dict) > 0)
})

test_that("multiword_expressions data object exists and has correct structure", {
  data("multiword_expressions", package = "quanteda.extras")
  
  expect_type(multiword_expressions, "character")
  expect_true(length(multiword_expressions) > 0)
  expect_true(all(nchar(multiword_expressions) > 0))
})
