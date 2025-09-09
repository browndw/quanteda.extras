# Global variables to avoid CMD check notes
utils::globalVariables(c(
  "token", "n", "freq", "id", "node_word", "col_freq",
  "Token", "MI_1", "PMI", "PMI2", "PMI3", "NPMI", ".data"
))

#' Calculate collocational associations by Mutual Information
#'
#' A function for calculating point-wise mutual information
#' from quanteda tokens.
#' The function requires:
#' - a tokens object
#' - a node word to search for
#' - a window counting to the left
#' - a window counting to the right
#' So `collocates_by_MI(my_tokens, "test", 5, 5)` would look for collocates
#' words to the left and 5 words to the right of the word "test".
#'
#' @param target_tkns The target quanteda tokens object.
#' @param node_word The token of interest.
#' @param left How many words a span should extend to the left of the node word.
#' @param right How many words a span should extend to the right of the
#'   node word.
#' @param statistic The type of mutual information to calculate. One of "pmi"
#'   (default), "pmi2", "pmi3", or "npmi".
#' @return A data.frame containing absolute frequencies and Mutual Information
#'   calculations. Observed counts are taken from tokens within the specified
#'   left/right window around the node, and expected probabilities are computed
#'   from corpus-wide token frequencies. PMI (MI1) follows the original
#'   corpus-total-based definition.
#' @importFrom utils head tail
#' @examples
#' \donttest{
#' library(quanteda)
#' texts <- c("The big cat sat on the mat",
#'            "The big cat ran very fast",
#'            "The big cat likes to hunt mice")
#' toks <- tokens(texts)
#' result <- collocates_by_MI(toks, "cat", left = 1, right = 1)
#' head(result)
#' }
#' @export
collocates_by_MI <- function(target_tkns, node_word, left = 5, right = 5,
                             statistic = c("pmi", "pmi2", "pmi3", "npmi")) {
  if (!inherits(target_tkns, "tokens")) {
    stop("Your target must be a quanteda tokens object.")
  }
  if (!is.numeric(left) || !is.numeric(right)) {
    stop("Your span must be numeric.")
  }
  if (left < 0) stop("Span values must be positive.")
  if (right < 0) stop("Span values must be positive.")
  if (left == 0 && right == 0) stop("The total span must be greater than 0.")
  # Validate statistic with a friendly error message expected by tests
  .choices <- c("pmi", "pmi2", "pmi3", "npmi")
  if (length(statistic) == 1 && !statistic %in% .choices) {
    stop("statistic must be one of: pmi, pmi2, pmi3, npmi")
  }
  statistic <- match.arg(statistic, .choices)
  # Set the span as the sum of our left and right window plus the node (legacy)
  # No longer used with kwic-based extraction, kept here for reference only.

  # Create a cleaned token stream for totals and window n-grams (exclude
  # punctuation-only tokens). Do this before n-gram generation to stay aligned.
  totals_tokens <- quanteda::tokens_remove(
    target_tkns,
    pattern = "^[[:punct:]]+$",
    valuetype = "regex",
    padding = FALSE
  )

  # Use KWIC to extract token contexts, which truncates at document edges.
  # We request a symmetric window of size max(left, right), and then trim
  # each side to the requested left/right sizes to support asymmetry.
  max_win <- max(left, right)
  kw <- suppressWarnings(
    quanteda::kwic(
      totals_tokens,
      pattern = node_word,
      window = max_win,
      valuetype = "fixed",
      case_insensitive = TRUE
    )
  )

  tokens_left <- NULL
  tokens_right <- NULL
  if (NROW(kw)) {
    # kw$pre and kw$post are space-separated strings of tokens
    if (left > 0) {
      pre_splits <- strsplit(kw$pre, " ", fixed = TRUE)
      # take the last `left` tokens (handles shorter contexts near boundaries)
      tokens_left <- unlist(lapply(pre_splits, function(x) {
        if (length(x) == 0) return(character(0))
        tail(x, left)
      }))
    }
    if (right > 0) {
      post_splits <- strsplit(kw$post, " ", fixed = TRUE)
      # take the first `right` tokens
      tokens_right <- unlist(lapply(post_splits, function(x) {
        if (length(x) == 0) return(character(0))
        head(x, right)
      }))
    }
  }

  # Generate total counts for all words in our corpus (exclude punctuation
  # from totals to align with expected PMI baselines) — use the same tokens
  # as above to ensure alignment
  totals <- suppressWarnings(
    quanteda.textstats::textstat_frequency(quanteda::dfm(totals_tokens))
  )

  # Create a frequency table and convert it to a data.frame.
  combined_tokens <- c(tokens_left, tokens_right)
  # Keep only alphabetic tokens to reduce artifacts from hyphens/numbers
  if (length(combined_tokens)) {
    combined_tokens <- tolower(combined_tokens)
    combined_tokens <- combined_tokens[grepl("^[a-z]+$", combined_tokens)]
  }

  # Handle case where no collocates are found
  if (length(combined_tokens) == 0) {
    col_freq <- data.frame(
      feature = character(0),
      c_freq = integer(0),
      stringsAsFactors = FALSE
    )
  } else {
    col_freq <- data.frame(
      table(tolower(combined_tokens)),
      stringsAsFactors = FALSE
    )
    # Only set column names if we have the expected structure
    if (ncol(col_freq) == 2) {
      colnames(col_freq) <- c("feature", "c_freq")
    } else {
      stop("Unexpected table structure: ", ncol(col_freq), " columns")
    }
  }

  # Ensure case consistency when merging with totals
  totals$feature <- tolower(totals$feature)
  # Merge our collocate frequencies with the corpus totals
  col_freq <- merge(col_freq, totals[, 1:2], by = "feature", all.x = TRUE)

  # Drop rows with missing totals (shouldn't occur after lowercasing)
  col_freq <- col_freq[!is.na(col_freq$frequency), , drop = FALSE]

  # If after merging no valid collocates remain, return empty result
  if (nrow(col_freq) == 0) {
    empty_result <- data.frame(
      token = character(0),
      col_freq = integer(0),
      total_freq = numeric(0),
      stringsAsFactors = FALSE
    )
    empty_result[[toupper(statistic)]] <- numeric(0)
    attr(empty_result, "node_word") <- node_word
    attr(empty_result, "corpus_total") <- sum(totals$frequency)
    empty_result <- structure(
      empty_result, class = c("collocations", "data.frame")
    )
    return(empty_result)
  }

  # Total corpus count (needed for expected probability of collocate)
  corpus_total <- sum(totals$frequency)
  # Frequency of node word in the corpus (case-insensitive)
  node_feat <- tolower(node_word)
  node_freq <- as.numeric(totals$frequency[match(node_feat, totals$feature)])
  if (is.na(node_freq)) node_freq <- 0

  # The function calculates different types of Mutual Information.
  # http://corpus.byu.edu/mutualInformation.asp
  #
  # M. Stubbs, Collocations and Semantic Profiles, Functions of Language 2,
  # 1 (1995)
  # MI: http://corpus.byu.edu/mutualInformation.asp

  # PMI (MI1) using corpus totals: PMI = log2( (c_freq/N) / ((c_total/N)*(node_freq/N)) )
  # which simplifies to log2( c_freq * N / (c_total * node_freq) )
  MI_pmi <- function(c_freq, c_total) {
    num <- c_freq * corpus_total
    denom <- c_total * node_freq
    score <- log2(num / denom)
    score[!is.finite(score)] <- NA_real_
    score
  }

  # PMI2 and PMI3 variants using corpus_total per original convention
  MI_pmi2 <- function(c_freq, c_total) {
    pmi <- MI_pmi(c_freq, c_total)
    res <- pmi + log2(c_freq / corpus_total)
    res[!is.finite(res)] <- NA_real_
    res
  }

  MI_pmi3 <- function(c_freq, c_total) {
    pmi <- MI_pmi(c_freq, c_total)
    res <- pmi + 2 * log2(c_freq / corpus_total)
    res[!is.finite(res)] <- NA_real_
    res
  }

  # NPMI using corpus_total
  MI_npmi <- function(c_freq, c_total) {
    pmi <- MI_pmi(c_freq, c_total)
    denom <- -log2(c_freq / corpus_total)
    res <- pmi / denom
    res[!is.finite(res)] <- NA_real_
    res
  }

  # Select the appropriate MI function based on statistic parameter
  mi_function <- switch(statistic,
                        "pmi" = MI_pmi,
                        "pmi2" = MI_pmi2,
                        "pmi3" = MI_pmi3,
                        "npmi" = MI_npmi)

  col_freq[[paste0(toupper(statistic))]] <- mi_function(
    col_freq$c_freq,
    col_freq$frequency
  )

  # Verify we have the expected number of columns before assigning names
  if (ncol(col_freq) == 4) {
    colnames(col_freq) <- c("token", "col_freq", "total_freq",
                            toupper(statistic))
  } else {
    stop("Unexpected number of columns in col_freq: ", ncol(col_freq))
  }
  # Deterministic ordering: sort by MI desc, then token asc for tie-break
  mi_col <- toupper(statistic)
  # Round MI for stable ordering under tiny numeric differences
  mi_sort <- round(col_freq[[mi_col]], 6)
  col_freq <- col_freq[order(
    -mi_sort,                      # primary: MI desc (rounded)
    col_freq$token                 # tie-break: token asc
  ), ]
  col_freq$token <- as.character(col_freq$token)
  rownames(col_freq) <- seq_len(nrow(col_freq))
  attr(col_freq, "node_word") <- node_word
  attr(col_freq, "corpus_total") <- corpus_total
  col_freq <- structure(col_freq, class = c("collocations", "data.frame"))
  col_freq
}

#' Create a table for plotting a collocational network
#'
#' This function operationalizes the idea of collcational networks described by
#' Brezina, McEnery & Wattam (2015). The function takes data.frames produced by
#' `collocates_by_MI()` and generates a tidygraph data object for plotting in
#' ggraph.
#'
#' @param col_1 A collocations object produced by collocates_by_MI().
#' @param ... Other collocations objects for plotting.
#' @importFrom rlang .data
#' @return A tidygraph table for network plotting.
#' @examples
#' \donttest{
#' library(quanteda)
#' texts <- c("The big cat sat on the mat",
#'            "The big cat ran very fast",
#'            "The big cat likes to hunt mice",
#'            "Time flies like an arrow",
#'            "Money may be important, but so is time")
#' toks <- tokens(texts)
#' # Create collocation objects for different node words
#' cat_collocations <- collocates_by_MI(toks, "cat", left = 1, right = 1)
#' time_collocations <- collocates_by_MI(toks, "time", left = 1, right = 1)
#' # Create network object
#' network <- col_network(cat_collocations, time_collocations)
#' print(network)
#' }
#' @export
#' @references Brezina, McEnery and Wattam (2015).
#'   "Collocations in context: A new perspective on collocation networks."
#'   *International Journal of Corpus Linguistics* 20 (2), 139-173.
#'   \doi{10.1075/ijcl.20.2.01bre}
col_network <- function(col_1, ...) {

  # put all the collocation data.frames into a list
  all_col <- list(col_1, ...)

  # check the object class
  test_class <- lapply(all_col, class)
  if (!all(sapply(test_class, "[[", 1) == "collocations")) {
    stop("Your data must be collocations objects.")
  }

  # extract the node words from attributes
  node_words <- lapply(all_col, function(x) attr(x, "node_word"))
  # extract the corpus totals from the attributes
  corpus_totals <- lapply(all_col, function(x) attr(x, "corpus_total"))

  # create an index
  idx <- seq_along(all_col)

  # normalize frequencies (guard empty) using base operations
  all_col <- lapply(
    idx,
    function(i) {
      df <- all_col[[i]]
      if (is.null(df) || nrow(df) == 0) return(df)
      df$col_freq <- df$col_freq / corpus_totals[[i]]
      df
    }
  )
  # add a column of node words by id; handle empty
  edges <- lapply(idx, function(i) {
    df <- all_col[[i]]
    if (is.null(df) || nrow(df) == 0) {
      data.frame(node_word = integer(0), token = character(0),
                 col_freq = numeric(0), total_freq = numeric(0),
                 stringsAsFactors = FALSE)
    } else {
      cbind(node_word = i, df)
    }
  })
  # bind all data.frames
  edges <- dplyr::bind_rows(edges)

  # create a vector of collocation tokens
  col_vect <- lapply(idx, function(i) {
    df <- all_col[[i]]
    if (is.null(df) || nrow(df) == 0) character(0) else df$token
  })
  # get unique tokens
  col_vect <- unique(unlist(col_vect))
  # and sort alphabetically
  col_vect <- sort(col_vect)

  # calculate the minimum id for collocations based on the number of node words
  id_min <- max(idx) + 1
  # calcuate the max id number
  id_max <- length(col_vect) + (id_min - 1)
  # generate ids for unique collocations
  col_id <- data.frame(
    id = seq(id_min, id_max), token = col_vect, stringsAsFactors = FALSE
  )
  # merge to create connections based on ids; keep only edges rows
  edges <- merge(edges, col_id, by = "token", all.x = TRUE)

  # Coalesce MI columns across possible statistics into a single MI value
  mi_candidates <- c("PMI", "PMI2", "PMI3", "NPMI")
  present_mi <- intersect(mi_candidates, names(edges))
  if (length(present_mi) == 0) {
    edges$MI <- numeric(nrow(edges))
  } else {
    mi_val <- rep(NA_real_, nrow(edges))
    for (nm in present_mi) {
      cur <- edges[[nm]]
      sel <- is.na(mi_val) & !is.na(cur)
      if (any(sel)) mi_val[sel] <- cur[sel]
    }
    mi_val[is.na(mi_val)] <- 0
    edges$MI <- mi_val
  }

  # group all collocates by id using base R
  if (nrow(edges) > 0) {
    intersects <- stats::aggregate(node_word ~ id, data = edges,
                                   FUN = function(x) length(unique(x)))
    names(intersects)[names(intersects) == "node_word"] <- "n"
    freq_norm <- stats::aggregate(col_freq ~ id, data = edges, FUN = mean)
    names(freq_norm)[names(freq_norm) == "col_freq"] <- "freq"
  } else {
    intersects <- data.frame(id = integer(0), n = integer(0))
    freq_norm <- data.frame(id = integer(0), freq = numeric(0))
  }
  # find the max frequency of all collocates
  freq_max <- max(freq_norm$freq)
  # make a data.frame of node words,
  # setting intersections to 0 and frequency to freq_max
  node_words <- data.frame(
    id = seq_along(idx), token = unlist(node_words), n = 0, freq = freq_max
  )

  # merge values into a single data.frame, containing all node information
  nodes <- merge(col_id, intersects, by = "id", all.x = TRUE)
  nodes <- merge(nodes, freq_norm, by = "id", all.x = TRUE)
  # add node_word values to top of data.frame
  nodes <- rbind(node_words, nodes)
  nodes$token <- as.character(nodes$token)
  nodes$n <- as.factor(nodes$n)
  names(nodes)[names(nodes) == "token"] <- "label"
  names(nodes)[names(nodes) == "n"] <- "n_intersects"
  names(nodes)[names(nodes) == "freq"] <- "node_weight"

  # assemble edge values using coalesced MI
  if (nrow(edges) > 0) {
    edges <- data.frame(
      to = edges$id,
      from = edges$node_word,
      link_weight = edges$MI,
      stringsAsFactors = FALSE
    )
  } else {
    edges <- data.frame(to = integer(0), from = integer(0),
                        link_weight = numeric(0), stringsAsFactors = FALSE)
  }

  # generate a tidygraph object for plotting
  col_net <- tidygraph::tbl_graph(nodes = nodes, edges = edges,
                                  directed = FALSE)
  col_net
}
