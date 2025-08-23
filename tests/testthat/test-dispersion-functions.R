test_that("dispersions_token works correctly", {
  skip_if_not_installed("quanteda")
  
  library(quanteda)
  test_text <- c("The cat sat on the mat", 
                 "The dog ran in the park", 
                 "A cat and dog played together")
  test_dfm <- dfm(tokens(test_text))
  
  # Test basic functionality
  result <- dispersions_token(test_dfm, "cat")
  
  expect_type(result, "list")
  expect_true("Absolute frequency" %in% names(result))
  expect_true("Range" %in% names(result))
  expect_true("Standard deviation" %in% names(result))
  expect_true("Deviation of proportions DP" %in% names(result))
})

test_that("dispersions_token input validation works", {
  skip_if_not_installed("quanteda")
  
  library(quanteda)
  test_text <- c("The cat sat")
  test_dfm <- dfm(tokens(test_text))
  
  # Test non-dfm object
  expect_error(dispersions_token("not dfm", "cat"), 
               "Your target must be a quanteda dfm object")
  
  # Test non-character token
  expect_error(dispersions_token(test_dfm, 123), 
               "Your token must be a character string")
  
  # Test token not found
  expect_error(dispersions_token(test_dfm, "nonexistent"), 
               "Token not found in document feature matrix")
})

test_that("dispersions_all works correctly", {
  skip_if_not_installed("quanteda")
  
  library(quanteda)
  test_text <- c("The cat sat on the mat", 
                 "The dog ran in the park")
  test_dfm <- dfm(tokens(test_text))
  
  # Test basic functionality
  result <- dispersions_all(test_dfm)
  
  expect_s3_class(result, "data.frame")
  expect_true("Token" %in% names(result))
  expect_true("AF" %in% names(result))
  expect_true("Carrolls_D2" %in% names(result))
  expect_true("DP" %in% names(result))
  expect_equal(nrow(result), quanteda::nfeat(test_dfm))
})

test_that("dispersions_all input validation works", {
  # Test non-dfm object
  expect_error(dispersions_all("not dfm"), 
               "Your target must be a quanteda dfm object")
})

test_that("ARF works correctly", {
  skip_if_not_installed("quanteda")
  
  library(quanteda)
  test_text <- c("The cat sat on the mat", 
                 "The dog ran in the park")
  test_tokens <- tokens(test_text)
  
  # Test basic functionality
  result <- ARF(test_tokens)
  
  expect_s3_class(result, "data.frame")
  expect_true("Token" %in% names(result))
  expect_true("ARF" %in% names(result))
  expect_true(all(result$ARF >= 0))
})

test_that("ARF input validation works", {
  # Test non-tokens object
  expect_error(ARF("not tokens"), 
               "ARF requires a quanteda tokens object")
})

test_that("frequency_table works correctly", {
  skip_if_not_installed("quanteda")
  
  library(quanteda)
  test_text <- c("The cat sat on the mat", 
                 "The dog ran in the park")
  test_tokens <- tokens(test_text)
  
  # Test basic functionality
  result <- frequency_table(test_tokens)
  
  expect_s3_class(result, "data.frame")
  expect_true("Token" %in% names(result))
  expect_true("AF" %in% names(result))
  expect_true("ARF" %in% names(result))
  expect_true("DP" %in% names(result))
})

test_that("frequency_table input validation works", {
  # Test non-tokens object
  expect_error(frequency_table("not tokens"), 
               "The function requires a quanteda tokens object")
})
