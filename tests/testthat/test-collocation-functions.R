test_that("collocates_by_MI works with different statistics", {
  skip_if_not_installed("quanteda")

  library(quanteda)
  test_text <- c("The big cat sat on the mat", 
                 "The big cat ran fast", 
                 "The big cat likes fish")
  test_tokens <- tokens(test_text)

  # Test different statistics
  result_pmi <- collocates_by_MI(test_tokens, "cat", left = 1, right = 1, statistic = "pmi")
  result_pmi2 <- collocates_by_MI(test_tokens, "cat", left = 1, right = 1, statistic = "pmi2") 
  result_pmi3 <- collocates_by_MI(test_tokens, "cat", left = 1, right = 1, statistic = "pmi3")
  result_npmi <- collocates_by_MI(test_tokens, "cat", left = 1, right = 1, statistic = "npmi")

  # Check that all return collocations objects
  expect_s3_class(result_pmi, c("collocations", "data.frame"))
  expect_s3_class(result_pmi2, c("collocations", "data.frame"))
  expect_s3_class(result_pmi3, c("collocations", "data.frame"))
  expect_s3_class(result_npmi, c("collocations", "data.frame"))

  # Check column names match the statistic
  expect_true("PMI" %in% names(result_pmi))
  expect_true("PMI2" %in% names(result_pmi2))
  expect_true("PMI3" %in% names(result_pmi3))
  expect_true("NPMI" %in% names(result_npmi))

  # Check all have same number of rows (same collocations found)
  expect_equal(nrow(result_pmi), nrow(result_pmi2))
  expect_equal(nrow(result_pmi), nrow(result_npmi))

  # Check correctness of token inclusions
  tokens <- unique(result_pmi$token)
  expect_setequal(tokens, c("big", "sat", "ran", "likes"))
})

test_that("collocates_by_MI handles words at beginning/end of sentences", {
  skip_if_not_installed("quanteda")
  library(quanteda)

  texts <- c("Time flies like an arrow",
             "Money may be important, but so is time")
  test_tokens <- tokens(texts)

  result_pmi <- collocates_by_MI(test_tokens, "time", left = 1, right = 1)

  tokens <- unique(result_pmi$token)
  expect_setequal(tokens, c("flies", "is"))
})

test_that("collocates_by_MI input validation works", {
  skip_if_not_installed("quanteda")

  library(quanteda)
  test_text <- c("The cat sat on the mat")
  test_tokens <- tokens(test_text)

  # Test non-tokens object
  expect_error(collocates_by_MI("not tokens", "cat"), 
               "Your target must be a quanteda tokens object")

  # Test non-numeric span
  expect_error(collocates_by_MI(test_tokens, "cat", left = "five"), 
               "Your span must be numeric")

  # Test negative span
  expect_error(collocates_by_MI(test_tokens, "cat", left = -1), 
               "Span values must be positive")

  # Test zero span
  expect_error(collocates_by_MI(test_tokens, "cat", left = 0, right = 0), 
               "The total span must be greater than 0")

  # Test invalid statistic. Error message not tested, as match.arg() says "The
  # error messages given are liable to change and did so in R 4.2.0. Do not test
  # them in packages."
  expect_error(collocates_by_MI(test_tokens, "cat", statistic = "invalid"))
})

test_that("collocates_by_MI works with edge cases", {
  skip_if_not_installed("quanteda")
  library(quanteda)

  # Node at sentence start: left=0, right=2 -> only right-side collocates
  texts_start <- c("Time flies quickly.")
  toks_start <- tokens(texts_start)
  res_start <- collocates_by_MI(toks_start, "time", left = 0, right = 2)
  expect_s3_class(res_start, c("collocations", "data.frame"))
  expect_setequal(unique(res_start$token), c("flies", "quickly"))

  # Node at sentence end: left=2, right=0 -> only left-side collocates
  texts_end <- c("Life is precious time")
  toks_end <- tokens(texts_end)
  res_end <- collocates_by_MI(toks_end, "time", left = 2, right = 0)
  expect_s3_class(res_end, c("collocations", "data.frame"))
  expect_setequal(unique(res_end$token), c("is", "precious"))

  # Node not present: returns empty collocations object with attributes set
  texts_none <- c("The cat sat on the mat")
  toks_none <- tokens(texts_none)
  res_none <- collocates_by_MI(toks_none, "unobtanium", left = 1, right = 1)
  expect_s3_class(res_none, c("collocations", "data.frame"))
  expect_equal(nrow(res_none), 0L)
  expect_equal(attr(res_none, "node_word"), "unobtanium")
  expect_true(is.numeric(attr(res_none, "corpus_total")))
})

test_that("col_network works with collocations objects", {
  skip_if_not_installed("quanteda")
  skip_if_not_installed("tidygraph")

  library(quanteda)
  test_text <- c("The big cat sat on the mat", 
                 "The big dog ran in the park",
                 "The big cat and big dog played together")
  test_tokens <- tokens(test_text)

  # Create collocation objects
  col1 <- collocates_by_MI(test_tokens, "cat", left = 1, right = 1, statistic = "pmi")
  col2 <- collocates_by_MI(test_tokens, "dog", left = 1, right = 1, statistic = "pmi")

  # Skip if no collocations found (edge case)
  skip_if(nrow(col1) == 0 || nrow(col2) == 0)

  # Test network creation
  result <- col_network(col1, col2)

  expect_s3_class(result, "tbl_graph")
  # Check that it has the expected tbl_graph structure
  expect_true(length(result) >= 1)
})

test_that("col_network input validation works", {
  skip_if_not_installed("quanteda")

  # Test with non-collocations object should fail
  expect_error(col_network(data.frame(x = 1:3)), 
               "Your data must be collocations objects")
})

test_that("PMI matches legacy implementation in a non-edge case", {
  skip_if_not_installed("quanteda")
  library(quanteda)

  # Simple corpus without punctuation and with middle-position node
  texts <- c(
    "the big cat sat on the mat",
    "the big cat ran fast",
    "the big cat likes fish"
  )
  toks <- tokens(texts)

  # Current implementation
  res <- collocates_by_MI(toks, "cat", left = 1, right = 1, statistic = "pmi")

  # Legacy-style PMI for the same setup
  span <- 3
  ngrams <- suppressWarnings(quanteda::tokens_ngrams(toks, n = span, concatenator = " "))
  ngrams_vec <- unlist(quanteda::as.list(ngrams, toks))
  # Select trigrams where the middle token is 'cat' and tabulate neighbors
  parts <- strsplit(ngrams_vec, " ", fixed = TRUE)
  parts <- Filter(function(x) length(x) == 3 && identical(tolower(x[2]), "cat"), parts)
  left_tokens <- if (length(parts)) vapply(parts, function(x) x[1], character(1)) else character(0)
  right_tokens <- if (length(parts)) vapply(parts, function(x) x[3], character(1)) else character(0)
  combined <- tolower(c(left_tokens, right_tokens))
  legacy_tab <- if (length(combined)) {
    df <- as.data.frame(table(combined), stringsAsFactors = FALSE)
    names(df) <- c("feature", "c_freq")
    df
  } else {
    data.frame(feature = character(0), c_freq = integer(0), stringsAsFactors = FALSE)
  }

  # Totals and node frequency (legacy way)
  totals <- suppressWarnings(quanteda.textstats::textstat_frequency(quanteda::dfm(toks)))
  corpus_total <- sum(totals$frequency)
  node_freq <- as.numeric(totals[totals$feature == "cat", "frequency"])

  # Merge to get c_total for each collocate present in legacy_tab
  legacy_df <- merge(legacy_tab, totals[, 1:2], by = "feature", all.x = TRUE)
  colnames(legacy_df)[colnames(legacy_df) == "frequency"] <- "total_freq"

  # Compute legacy PMI
  legacy_df$PMI_legacy <- log2((legacy_df$c_freq / corpus_total) /
                                 ((legacy_df$total_freq / corpus_total) * (node_freq / corpus_total)))

  # Compare to current PMI for the overlapping token set
  # Focus on tokens known to appear in this corpus
  target_tokens <- c("big", "sat", "ran", "likes")
  cur_sub <- res[res$token %in% target_tokens, c("token", "PMI")]
  leg_sub <- legacy_df[legacy_df$feature %in% target_tokens, c("feature", "PMI_legacy")]
  cur_sub <- cur_sub[order(cur_sub$token), ]
  leg_sub <- leg_sub[order(leg_sub$feature), ]
  expect_equal(cur_sub$token, leg_sub$feature)
  expect_equal(cur_sub$PMI, leg_sub$PMI_legacy, tolerance = 1e-12)
})
