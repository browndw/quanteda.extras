test_that("keyness_table works correctly", {
  skip_if_not_installed("quanteda")
  
  library(quanteda)
  target_text <- c("The cat sat on the mat", "The cat ran quickly")
  reference_text <- c("The dog ran in the park", "The dog played outside")
  
  target_dfm <- dfm(tokens(target_text))
  reference_dfm <- dfm(tokens(reference_text))
  
  # Test basic functionality
  result <- keyness_table(target_dfm, reference_dfm)
  
  expect_s3_class(result, "data.frame")
  expect_true("Token" %in% names(result))
  expect_true("LL" %in% names(result))
  expect_true("LR" %in% names(result))
  expect_true("PV" %in% names(result))
  expect_true(any(grepl("_Tar", names(result))))
  expect_true(any(grepl("_Ref", names(result))))
})

test_that("keyness_table input validation works", {
  skip_if_not_installed("quanteda")
  
  library(quanteda)
  test_text <- c("The cat sat")
  test_dfm <- dfm(tokens(test_text))
  
  # Test non-dfm objects
  expect_error(keyness_table("not dfm", test_dfm), 
               "Your target must be a quanteda dfm object")
  expect_error(keyness_table(test_dfm, "not dfm"), 
               "Your reference must be a quanteda dfm object")
})

test_that("key_keys works correctly", {
  skip_if_not_installed("quanteda")
  
  library(quanteda)
  target_text <- c("The cat sat on the mat", "The cat ran quickly", "Another cat story")
  reference_text <- c("The dog ran in the park", "The dog played outside")
  
  target_dfm <- dfm(tokens(target_text))
  reference_dfm <- dfm(tokens(reference_text))
  
  # Test basic functionality
  result <- key_keys(target_dfm, reference_dfm)
  
  expect_s3_class(result, "data.frame")
  expect_true("token" %in% names(result))
  expect_true("key_range" %in% names(result))
  expect_true("key_mean" %in% names(result))
  expect_true("key_sd" %in% names(result))
  expect_true("effect_mean" %in% names(result))
})

test_that("key_keys input validation works", {
  skip_if_not_installed("quanteda")
  
  library(quanteda)
  test_text <- c("The cat sat")
  test_dfm <- dfm(tokens(test_text))
  
  # Test non-dfm objects
  expect_error(key_keys("not dfm", test_dfm), 
               "Your target must be a quanteda dfm object")
  expect_error(key_keys(test_dfm, "not dfm"), 
               "Your reference must be a quanteda dfm object")
})

test_that("keyness_pairs works correctly", {
  skip_if_not_installed("quanteda")
  
  library(quanteda)
  text1 <- c("The cat sat on the mat")
  text2 <- c("The dog ran in the park")
  text3 <- c("The bird flew in the sky")
  
  dfm1 <- dfm(tokens(text1))
  dfm2 <- dfm(tokens(text2))
  dfm3 <- dfm(tokens(text3))
  
  # Test basic functionality with 3 dfms
  result <- keyness_pairs(dfm1, dfm2, dfm3)
  
  expect_s3_class(result, "data.frame")
  expect_true("Token" %in% names(result))
  expect_true(any(grepl("_LL$", names(result))))
  expect_true(any(grepl("_LR$", names(result))))
  expect_true(any(grepl("_PV$", names(result))))
})

test_that("keyness_pairs input validation works", {
  skip_if_not_installed("quanteda")
  
  library(quanteda)
  test_text <- c("The cat sat")
  test_dfm <- dfm(tokens(test_text))
  
  # Test non-dfm objects
  expect_error(keyness_pairs("not dfm", test_dfm), 
               "Your corpora must be quanteda dfm objects")
  expect_error(keyness_pairs(test_dfm, "not dfm"), 
               "Your corpora must be quanteda dfm objects")
})
