# Global variables to avoid CMD check notes
utils::globalVariables(c("token", "n", "freq", 
                         "Token", "MI_1", "PMI", "PMI2", "PMI3", "NPMI"))

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
#'   calculations.
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

  statistic <- match.arg(statistic)

  # Set the span as the sum of our left and right window plus the node
  span <- left + right + 1

  # Create regular expressions for use later - handle boundary cases
  if (left > 0 && right > 0) {
    # Allow for cases where the node word is at sentence boundaries
    search_exp <- paste0("(^|\\S+\\s)", node_word, "(\\s\\S+|$)")
  }
  if (left == 0) {
    search_exp <- paste0("^", node_word, "(\\s\\S+)", "{", right, "}")
  }
  if (right == 0) {
    search_exp <- paste0("(\\S+\\s)", "{", left, "}", node_word, "$")
  }

  # Generate ngrams the size of the span
  n_grams <- suppressWarnings(
    quanteda::tokens_ngrams(target_tkns, n = span, concatenator = " ")
  )
  # Convert ngrams to a vector
  n_grams <- unlist(quanteda::as.list(n_grams, target_tkns))

  # Subset the vector to include only ones that contain only the node word
  # first with a fast search then using our regular expression
  n_grams <- n_grams[stringr::str_detect(
    n_grams, stringr::regex(node_word, ignore_case = TRUE)
  )]
  n_grams <- n_grams[stringr::str_detect(
    n_grams, stringr::regex(search_exp, ignore_case = TRUE)
  )]

  # Extract words to the left and right of the node word
  if (left > 0) {
    tokens_left <- stringr::word(n_grams, start = 1, end = left,
                                 sep = stringr::fixed(" "))
  }
  if (right > 0) {
    tokens_right <- stringr::word(n_grams, start = span - right + 1, end = -1,
                                  sep = stringr::fixed(" "))
  }

  # Create vectors of all words occurring in our span.
  if (left > 0) {
    tokens_left <- unlist(strsplit(tokens_left, split = " "))
  }
  if (left == 0) tokens_left <- NULL
  if (right > 0) {
    tokens_right <- unlist(strsplit(tokens_right, split = " "))
  }
  if (right == 0) tokens_right <- NULL

  # Generate total counts for all words in our corpus.
  totals <- suppressWarnings(
    quanteda.textstats::textstat_frequency(quanteda::dfm(target_tkns))
  )

  # Create a frequency table and convert it to a data.frame.
  combined_tokens <- c(tokens_left, tokens_right)

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

  # Merge our collocate frequencies with the corpus totals
  col_freq <- merge(col_freq, totals[, 1:2], by = "feature", all.x = TRUE)

  # If no collocates were found, return an empty result
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

  # Find the frequency of our node word and the total corpus count.
  node_freq <- as.numeric(
    totals[stringr::str_detect(
      totals$feature,
      stringr::regex(paste0("^", node_word, "$"), ignore_case = TRUE)
    ), 2]
  )
  corpus_total <- sum(totals$frequency)

  # The function calculates different types of Mutual Information.
  # http://corpus.byu.edu/mutualInformation.asp
  #
  # M. Stubbs, Collocations and Semantic Profiles, Functions of Language 2,
  # 1 (1995)
  # MI: http://corpus.byu.edu/mutualInformation.asp

  # PMI (Pointwise Mutual Information) - original MI1
  MI_pmi <- function(c_freq, c_total) {
    mi_score <- log2((c_freq / corpus_total) /
                       ((c_total / corpus_total) *
                          (node_freq / corpus_total)))
    mi_score
  }

  # PMI2 - PMI minus log2(freq_span/corpus_total)
  MI_pmi2 <- function(c_freq, c_total) {
    pmi <- log2((c_freq / corpus_total) /
                  ((c_total / corpus_total) * (node_freq / corpus_total)))
    pmi2 <- pmi - log2(c_freq / corpus_total) * (-1)
    pmi2
  }

  # PMI3 - PMI minus 2*log2(freq_span/corpus_total)
  MI_pmi3 <- function(c_freq, c_total) {
    pmi <- log2((c_freq / corpus_total) /
                  ((c_total / corpus_total) *
                     (node_freq / corpus_total)))
    pmi3 <- pmi - log2(c_freq / corpus_total) * (-2)
    pmi3
  }

  # NPMI (Normalized PMI) - PMI divided by -log2(freq_span/corpus_total)
  MI_npmi <- function(c_freq, c_total) {
    pmi <- log2((c_freq / corpus_total) /
                  ((c_total / corpus_total) * (node_freq / corpus_total)))
    npmi <- pmi / (-log2(c_freq / corpus_total))
    npmi
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
  col_freq <- col_freq[order(-col_freq[[4]]), ]
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

  # normalize frequencies
  all_col <- lapply(
    idx,
    function(i) {
      dplyr::mutate(all_col[[i]],
                    col_freq = .data$col_freq / corpus_totals[[i]])
    }
  )
  # add a column of node words by id
  edges <- lapply(idx, function(i) cbind(node_word = i, all_col[[i]]))
  # bind all data.frames
  edges <- dplyr::bind_rows(edges)

  # create a vector of collocation tokens
  col_vect <- lapply(idx, function(i) (all_col[[i]]$token))
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
  # merge to create connections based on ids
  edges <- merge(edges, col_id, by = "token", all = TRUE)

  # group all collocates by id
  id_grp <- dplyr::group_by(edges, .data$id)
  # calculate the number of node words each collocation intersects with
  intersects <- dplyr::tally(id_grp)
  # for collocates with multiple intersections, summarize by mean
  freq_norm <- dplyr::summarise(id_grp, freq = mean(.data$col_freq))
  # find the max frequency of all collocates
  freq_max <- max(freq_norm$freq)
  # make a data.frame of node words,
  # setting intersections to 0 and frequency to freq_max
  node_words <- data.frame(
    id = seq_along(idx), token = unlist(node_words), n = 0, freq = freq_max
  )

  # merge values into a single data.frame, containing all node information
  nodes <- Reduce(
    function(x, y) merge(x, y, by = "id"), list(col_id, intersects, freq_norm)
  )
  # add node_word values to top of data.frame
  nodes <- rbind(node_words, nodes)
  nodes$token <- as.character(nodes$token)
  nodes$n <- as.factor(nodes$n)
  nodes <- dplyr::rename(
    nodes, label = .data$token, n_intersects = .data$n, node_weight = .data$freq
  )

  # assemble edge values - use the 4th column which contains the MI statistic
  # get the MI column name (PMI, PMI2, PMI3, or NPMI)
  mi_col_name <- names(edges)[4]
  edges <- dplyr::select(
    edges, .data$id, .data$node_word, !!rlang::sym(mi_col_name)
  )
  edges <- dplyr::rename(
    edges, to = .data$id, from = .data$node_word, link_weight = !!rlang::sym(mi_col_name)
  )

  # generate a tidygraph object for plotting
  col_net <- tidygraph::tbl_graph(nodes = nodes, edges = edges,
                                  directed = FALSE)
  col_net
}
