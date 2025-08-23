test_that("readtext_lite works correctly", {
  # Create temporary test files
  temp_dir <- tempdir()
  file1 <- file.path(temp_dir, "test1.txt")
  file2 <- file.path(temp_dir, "test2.txt")
  
  writeLines("This is test file 1.", file1)
  writeLines(c("Line 1 of test file 2.", "Line 2 of test file 2."), file2)
  
  # Test the function
  result <- readtext_lite(c(file1, file2))
  
  # Check structure
  expect_s3_class(result, c("readtext", "data.frame"))
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 2)
  expect_equal(names(result), c("doc_id", "text"))
  
  # Check content
  expect_equal(result$doc_id, c("test1.txt", "test2.txt"))
  expect_equal(result$text[1], "This is test file 1.")
  expect_equal(result$text[2], "Line 1 of test file 2.\nLine 2 of test file 2.")
  
  # Clean up
  unlink(c(file1, file2))
})

test_that("preprocess_text works with default parameters", {
  text <- "Hello World! It's a test-case with numbers 123."
  result <- preprocess_text(text)
  
  # Should be lowercase, contractions separated, hyphens replaced, punctuation removed
  expected <- "hello world it s a test case with numbers 123"
  expect_equal(result, expected)
})

test_that("preprocess_text works with different parameters", {
  text <- "Hello World! It's a test-case with numbers 123."
  
  # Test with contractions = FALSE
  result1 <- preprocess_text(text, contractions = FALSE)
  expect_true(grepl("it's", result1))
  
  # Test with hypens = FALSE
  result2 <- preprocess_text(text, hypens = FALSE)
  expect_true(grepl("test-case", result2))
  
  # Test with punctuation = FALSE
  result3 <- preprocess_text(text, punctuation = FALSE)
  expect_true(grepl("!", result3))
  
  # Test with lower_case = FALSE
  result4 <- preprocess_text(text, lower_case = FALSE)
  expect_true(grepl("Hello", result4))
  
  # Test with remove_numbers = TRUE
  result5 <- preprocess_text(text, remove_numbers = TRUE)
  expect_false(grepl("123", result5))
})

test_that("excel_style works correctly", {
  expect_equal(excel_style(1), "A")
  expect_equal(excel_style(26), "Z")
  expect_equal(excel_style(27), "AA")
  expect_equal(excel_style(c(1, 2, 3)), c("A", "B", "C"))
  expect_equal(excel_style(702), "ZZ")
})

test_that("normalizing_factor works correctly", {
  # Test various corpus sizes
  expect_equal(normalizing_factor(1000), c(Per_10.3 = 1000))
  expect_equal(normalizing_factor(10000), c(Per_10.4 = 10000))
  expect_equal(normalizing_factor(1000000), c(Per_10.6 = 1000000))
  expect_equal(normalizing_factor(10000000), c(Per_10.6 = 1000000))  # Cap at 10^6
  expect_equal(normalizing_factor(50), c(Per_10.2 = 100))  # Minimum 100
})
