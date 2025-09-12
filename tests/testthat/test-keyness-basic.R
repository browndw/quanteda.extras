test_that("log_like works correctly", {
  # Basic test
  result <- log_like(10, 5, 100, 200)
  expect_type(result, "double")
  expect_length(result, 1)
  expect_equal(result, 6.931472, tolerance = 0.1)
  
  # Test with Yates correction
  result_corrected <- log_like(10, 5, 100, 200, correct = TRUE)
  expect_type(result_corrected, "double")
  expect_length(result_corrected, 1)
  
  # Test edge cases
  expect_equal(log_like(0, 5, 100, 200), log_like(0, 5, 100, 200, correct = FALSE))
  expect_equal(log_like(10, 0, 100, 200), log_like(10, 0, 100, 200, correct = FALSE))
})

test_that("log_ratio works correctly", {
  # Basic test
  result <- log_ratio(10, 5, 100, 200)
  expect_type(result, "double")
  expect_length(result, 1)
  
  # Test with zero values (should handle edge cases)
  result_zero <- log_ratio(0, 5, 100, 200)
  expect_type(result_zero, "double")
  expect_length(result_zero, 1)
  
  # Test vectorized input
  result_vector <- log_ratio(c(10, 20), c(5, 15), c(100, 150), c(200, 300))
  expect_type(result_vector, "double")
  expect_length(result_vector, 2)
})
