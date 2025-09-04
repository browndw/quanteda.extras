#' Log-likelihood calculation
#'
#' Log-likelihood tests the frequencies of tokens in one corpus vs. another. It
#' is often used instead of a chi-square test, as it has been shown to be more
#' resistant to corpora of varying sizes. For more detail see:
#' <http://ucrel.lancs.ac.uk/llwizard.html>
#'
#' @param n_target The non-normalized token count in the target corpus
#' @param n_reference The non-normalized token count in the reference corpus
#' @param total_target The total number of tokens in the target corpus
#' @param total_reference The total number of tokens in the reference corpus
#' @param correct Whether to perform the Yates correction
#' @return A numeric value representing log-likelihood
#' @export
log_like <- function(n_target, n_reference, total_target, total_reference,
                     correct = FALSE) {
  a <- n_target
  b <- total_target - n_target
  c <- n_reference
  d <- total_reference - n_reference

  expected_a <- (n_target + n_reference) *
    (a + c) / (a + b + c + d)
  expected_b <- (n_target + n_reference) *
    (b + d) / (a + b + c + d)
  if (correct == TRUE) { # Perform the "Yates" correction
    n_a <- ifelse(n_target - expected_a > 0.25, n_target - 0.5, n_target)
    n_b <- ifelse(n_target - expected_a > 0.25,  n_reference + 0.5, n_reference)

    n_a <- ifelse(expected_a - n_target > 0.25, n_target + 0.5, n_a)
    n_b <- ifelse(expected_a - n_target > 0.25, n_reference - 0.5, n_b)
  } else {
    n_a <- n_target
    n_b <- n_reference
  }
  l1 <- ifelse(n_a == 0, 0, n_a * log(n_a / expected_a))
  l2 <- ifelse(n_b == 0, 0, n_b * log(n_b / expected_b))
  likelihood <- 2 * (l1 + l2)
  likelihood <- ifelse(n_target / total_target > n_reference / total_reference,
                       likelihood, -likelihood)
  likelihood
}

#' Log-ratio calculation
#'
#' Take a target column and a reference column, and return an effect size. This
#' effect size calculation is called Log Ratio and was proposed by Andrew
#' Hardie: <http://cass.lancs.ac.uk/log-ratio-an-informal-introduction/>
#'
#' @details
#' Let \eqn{n_\text{target}}{n_target} and \eqn{n_\text{ref}}{n_ref} be the
#' counts of the word in the target and reference corpora, and let
#' \eqn{C_\text{target}}{C_target} and \eqn{C_\text{ref}}{C_ref} be the total
#' number of words in the target and reference corpora. The log-ratio is then
#' \deqn{\log_2\left( \frac{n_\text{target} / C_\text{target}}{n_\text{ref} /
#' C_\text{ref}} \right).}{log_2 ((n_target / C_target) / (n_ref / C_ref)).}
#' A log-ratio of 0 indicates an equal rate of use in each corpus.
#'
#' The `n_target` and `n_reference` arguments can be vectors of counts of
#' multiple tokens, provided they are in the same order.
#' @param n_target The non-normalized token count in the target corpus
#' @param n_reference The non-normalized token count in the reference corpus
#' @param total_target The total number of tokens in the target corpus
#' @param total_reference The total number of tokens in the reference corpus
#' @return The log ratio
#' @export
log_ratio <- function(n_target, n_reference, total_target, total_reference) {
  total_a <- sum(n_target)
  total_b <- sum(n_reference)
  percent_a <- ifelse(n_target == 0, 0.5 / total_a, n_target / total_target)
  percent_b <- ifelse(n_reference == 0, 0.5 / total_b,
                      n_reference / total_reference)
  ratio <- log2(percent_a / percent_b)
  ratio
}

#' Key of keys calculation
#'
#' The following function is based on an idea proposed by Mike Scott
#' and used in his concordancer WordSmith:
#' https://lexically.net/downloads/version4/html/index.html?database_info.htm
#' Rather than summing counts from all texts in the target corpus
#' and comparing them to those in a reference corpus,
#' Scott proposes to iterate through each text in the target corpus,
#' calculating keyness values against the reference corpus.
#' Then you find how many texts reach some significance threshold.
#' Essentially, this is a way of accounting for distribution:
#' Are a few texts driving keyness values? Or many?
#' The function returns a data.frame that includes:
#' - the percent of texts in the target corpus for which keyness reaches
#'   the specified threshold
#' - the mean keyness value in the target
#' - the standard deviation of keyness
#' - the mean effect size by log ratio
#' Note that it is easy enough to alter the function to return other values.
#'
#' @param target_dfm The target document-feature matrix
#' @param reference_dfm The reference document-feature matrix
#' @param threshold The p-value threshold for reaching significance
#' @param yates A logical value indicating whether to perform the "Yates"
#'   correction
#' @return A data.frame containing the percentage of documents reaching
#'   significance, mean keyness, and mean effect size
#' @examples
#' \donttest{
#' library(quanteda)
#' # Create target corpus (political texts)
#' target_texts <- c("The government policy was controversial",
#'                   "Political debates are heated", 
#'                   "Policy makers need evidence")
#' # Create reference corpus (general texts)  
#' reference_texts <- c("The cat sat on the mat",
#'                      "Dogs love to play in parks",
#'                      "Animals need care and attention")
#' target_dfm <- dfm(tokens(target_texts))
#' reference_dfm <- dfm(tokens(reference_texts))
#' key_keys_result <- key_keys(target_dfm, reference_dfm)
#' head(key_keys_result)
#' }
#' @importFrom stats qchisq
#' @export
key_keys <- function(target_dfm, reference_dfm, threshold = 0.05,
                     yates = FALSE) {
  if (!inherits(target_dfm, "dfm")) {
    stop("Your target must be a quanteda dfm object.")
  }
  if (!inherits(reference_dfm, "dfm")) {
    stop("Your reference must be a quanteda dfm object.")
  }  # Here we just specify the thresholds for p-values with df = 1
  th <- qchisq(1 - threshold, df = 1)

  # Here, we're just restructuring our data.
  # First, we're making sure it's trimmed.
  target_dfm <- suppressWarnings(
    quanteda::dfm_trim(target_dfm, min_termfreq = 1)
  )
  reference_dfm <- suppressWarnings(
    quanteda::dfm_trim(reference_dfm, min_termfreq = 1)
  )

  # Sum the frequencies for the reference corpus.
  reference_df <- suppressWarnings(
    quanteda.textstats::textstat_frequency(reference_dfm)
  )

  # Prep the target corpus by first converting it to a data.frame
  target_df <- suppressWarnings(
    quanteda::convert(target_dfm, to = "data.frame")
  )
  target_docs <- target_df$doc_id
  # There are other ways of doing this, but we need to make sure
  # our data contains ALL tokens for both the target and reference corpora.
  # In base R, we first transform the data.frame.
  target_df <- as.data.frame(t(target_df), stringsAsFactors = FALSE)
  # Then rearrange our row names and column names.
  target_df <- tibble::rownames_to_column(target_df, "feature")
  colnames(target_df) <- c("feature", target_docs)
  target_df <- target_df[-1, ]
  # Drop the unnecssary columns from our reference.
  reference_df <- reference_df[, -c(3:5)]
  # Now we merge both into a combined data.frame specifying all = T.
  comb_df <- merge(reference_df, target_df, by = "feature", all = TRUE)
  # Convert NAs into zeros.
  comb_df[is.na(comb_df)] <- 0
  # Now we create a vector of our reference counts
  # from a column called "frequency".
  feature <- comb_df$feature
  reference_counts <- comb_df$frequency
  # Drop columns to create the data.frame of our target counts.
  target_counts <- comb_df[, -c(1:2)]
  # Convert from character to numeric values.
  target_counts <- as.data.frame(sapply(target_counts, as.numeric))

  target_totals <- as.vector(colSums(target_counts))
  reference_total <- sum(reference_counts)

  # This generates an index of columns that we can iterate through.
  idx <- seq_len(ncol(target_counts))

  # This iterates through the columns and
  # generates a data.frame of keyness values.

  keyness <- as.data.frame(
    sapply(idx, function(i) {
      log_like(
        target_counts[, i], reference_counts, target_totals[i],
        reference_total, correct = yates
      )
    })
  )

  # We're also going to generate a data.frame of effect sizes.
  effect <- as.data.frame(
    sapply(idx, function(i) {
      log_ratio(
        target_counts[, i], reference_counts, target_totals[i],
        reference_total
      )
    })
  )
  rownames(effect) <- feature

  # From these two data.frames we can generate some values.
  #
  # The mean effect sizes:
  effect_mean <- apply(effect, 1, mean)
  # The percentages of texts that reach the p-value threshold:
  key_range <- apply(
    keyness, 1,
    function(x) (length(which(x > th)) / ncol(keyness)) * 100
  )
  # The mean keyness values:
  key_mean <- apply(keyness, 1, mean)
  # Standard deviations for keyness values:
  key_sd <- apply(keyness, 1, sd)
  # Combine those into a report.
  report <- data.frame(cbind(key_range, key_mean, key_sd, effect_mean))
  # Order the report by mean keyness.
  report <- report[order(-report$key_mean), ]
  report <- tibble::rownames_to_column(report, "token")
  # And return the report.
  report
}

#' Pairwise keyness values from any number of dfms
#'
#' This function takes any number of quanteda dfm objects and
#' returns a table of log-likelihood values, effect sizes
#' using Hardie's log ratio and p-values
#'
#' @param dfm_a A document-feature matrix
#' @param dfm_b A document-feature matrix
#' @param ... Additional document-feature matrices
#' @param yates Whether to perform the "Yates" correction
#' @return A data.frame containing pairwise keyness comparisons of all dfms
#' @export
#' @importFrom stats pchisq
#' @importFrom utils combn
keyness_pairs <- function(dfm_a, dfm_b, ..., yates = FALSE) {
  all_corpora <- list(dfm_a, dfm_b, ...)
  test_class <- unlist(lapply(all_corpora, function(c) inherits(c, "dfm")))
  if (!all(test_class)) {
    stop("Your corpora must be quanteda dfm objects.")
  }

  # Generate frequency lists using textstat_frequency()
  freq_list <- lapply(
    all_corpora,
    suppressWarnings(quanteda.textstats::textstat_frequency)
  )

  # Subset out the need columns
  freq_list <- lapply(
    freq_list,
    function(x) subset(x, select = c("feature", "frequency"))
  )
  # Create an index
  idx <- seq_along(all_corpora)
  # Rename columns so they will be unique using data.table
  new_names <- lapply(
    idx,
    function(i) paste("V", i, sep = "_")
  )
  freq_list <- lapply(idx, function(i) {
    data.table::setnames(
      freq_list[[i]],
      "frequency",
      new_names[[i]]
    )
  })
  # Merge all lists by feature.
  # This ensures that we have ALL features from ALL dfms
  # even in counts in one or more may be zero
  freq_df <- Reduce(
    function(...) merge(..., by = "feature", all = TRUE),
    freq_list
  )
  freq_df <- data.frame(freq_df[, -1], row.names = freq_df[, 1])
  # Set NA values to zero for calcutaions
  freq_df[is.na(freq_df)] <- 0
  # Create an index of pairs
  corpora_pairs <- combn(idx, 2)
  pair_idx <- seq_len(ncol(corpora_pairs))
  # Get the total counts
  total_counts <- colSums(freq_df)
  comp_names <- sapply(pair_idx, function(i) {
    name_pairs <- combn(excel_style(idx), 2)
    j <- name_pairs[1, i]
    k <- name_pairs[2, i]
    paste(j, k, sep = "_v_")
  })

  # Calculate log-likeihood
  ll <- as.data.frame(sapply(pair_idx, function(i) {
    j <- corpora_pairs[1, i]
    k <- corpora_pairs[2, i]
    log_like(
      freq_df[, j], freq_df[, k],
      total_counts[j], total_counts[k],
      correct = yates
    )
  }))
  # Apply column names
  colnames(ll) <- lapply(comp_names, function(x) paste(x, "LL", sep = "_"))
  # Calculate the effect sizes
  lr <- as.data.frame(sapply(pair_idx, function(i) {
    j <- corpora_pairs[1, i]
    k <- corpora_pairs[2, i]
    log_ratio(freq_df[, j], freq_df[, k], total_counts[j], total_counts[k])
  }))
  # Apply column names
  colnames(lr) <- lapply(comp_names, function(x) paste(x, "LR", sep = "_"))
  # Calculate p-values
  pv <- as.data.frame(
    sapply(pair_idx, function(i) {
      mapply(function(x) pchisq(abs(x), 1, lower.tail = FALSE), ll[, i])
    })
  )
  # Apply column names
  colnames(pv) <- lapply(comp_names, function(x) paste(x, "PV", sep = "_"))
  # Assemble the table of all values
  key_table <- cbind(ll, lr, pv)
  # Order by names
  key_table <- key_table[, order(names(key_table))]
  # Add rownames from the frequency counts
  rownames(key_table) <- rownames(freq_df)
  # Reorder by the first column
  key_table <- key_table[order(key_table[, 1], decreasing = TRUE), ]
  key_table <- tibble::rownames_to_column(key_table, "Token")
  # Return the table
  key_table
}

#' Keyness measures for all tokens in a corpus
#'
#' The keyness_table() function returns the log-likelihood
#' of the target vs. reference corpus, effect sizes by log ratio,
#' p-values, absolute frequencies, relative frequencies,
#' and deviation of proportions.
#'
#' @param target_dfm The target document-feature matrix
#' @param reference_dfm The reference document-feature matrix
#' @param yates A logical value indicating whether to perform the "Yates"
#'   correction
#' @return A data.frame containing the log-likelihood, log ratio, absolute
#'   frequencies, relative frequencies, and dispersions
#' @examples
#' \donttest{
#' library(quanteda)
#' target_texts <- c("The government policy was controversial",
#'                   "Political debates are heated",
#'                   "Policy makers need to consider evidence")
#' reference_texts <- c("The cat sat on the mat",
#'                      "Dogs love to play in parks",
#'                      "Animals need care and attention")
#' target_dfm <- dfm(tokens(target_texts))
#' reference_dfm <- dfm(tokens(reference_texts))
#' result <- keyness_table(target_dfm, reference_dfm)
#' head(result)
#' }
#' @export
keyness_table <- function(target_dfm, reference_dfm, yates = FALSE) {
  if (class(target_dfm)[1] != "dfm") {
    stop("Your target must be a quanteda dfm object.")
  }
  if (class(reference_dfm)[1] != "dfm") {
    stop("Your reference must be a quanteda dfm object.")
  }

  total_counts <- c(
    sum(quanteda::ntoken(target_dfm)),
    sum(quanteda::ntoken(reference_dfm))
  )
  nf <- normalizing_factor(max(total_counts))

  freq_table <- function(dfm) {

    m <- as.matrix(dfm)
    idx <- seq_len(ncol(m))
    total <- sum(rowSums(m))
    # calculte the relative sizes of the parts of the corpus (in percent)
    parts <- rowSums(m) / total

    dp <- function(v, s = rep(1 / length(v))) {
      f <- sum(v) # f
      s <- s / sum(s) # s

      values <- list()
      values[["AF"]] <- f
      values[[names(nf)]] <- (f / total) * as.numeric(nf)
      values[["DP"]] <- sum(abs((v / f) - s)) / 2
      values <- as.data.frame(t(as.matrix(unlist(values))))
      return(values)
    }

    dsp <- lapply(idx, function(i) {
      dp(m[, i], parts)
    })
    dsp <- data.frame(data.table::rbindlist(dsp))
    dsp$Token <- colnames(m)
    dsp <- dsp[order(-dsp$AF), ]
    dsp
  }

  target_df <- freq_table(target_dfm)
  reference_df <- freq_table(reference_dfm)

  freq_df <- merge(target_df, reference_df, by = "Token", all = TRUE)
  freq_df <- freq_df[, c(1, 2, 5, 3, 6, 4, 7)]
  freq_df[, 2:5][is.na(freq_df[, 2:5])] <- 0
  colnames(freq_df) <- gsub("\\.x", "_Tar", colnames(freq_df))
  colnames(freq_df) <- gsub("\\.y", "_Ref", colnames(freq_df))

  ll <- log_like(
    freq_df[, 2], freq_df[, 3], total_counts[1], total_counts[2],
    correct = yates
  )
  lr <- log_ratio(
    freq_df[, 2], freq_df[, 3], total_counts[1], total_counts[2]
  )
  pv <- pchisq(abs(ll), 1, lower.tail = FALSE)

  freq_df$LL <- ll
  freq_df$LR <- lr
  freq_df$PV <- pv
  freq_df <- freq_df[, c(1, 8:10, 2:7)]
  freq_df <- freq_df[order(freq_df[, 2], decreasing = TRUE), ]
  rownames(freq_df) <- seq_len(nrow(freq_df))
  return(freq_df)
}
