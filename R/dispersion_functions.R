#' Dispersion measures for a token
#'
#' The dispersions_token() function calculates
#' the dispersion measures for a single token.
#' For example: dispersions_token(my_dfm, "cat")
#' It returns a named list with all of the
#' dispersion measures discussed by S.T. Gries:
#' (http://www.stgries.info/research/dispersion/links.html)
#'
#' @param target_dfm The target document-feature matrix.
#' @param token The token for which dispersion measures are to be calculated.
#' @return A named list of dispersion measures.
#' @export
#' @importFrom stats sd
dispersions_token <- function(target_dfm, token) {

  if (!inherits(target_dfm, "dfm")) {
    stop("Your target must be a quanteda dfm object.")
  }
  if (!inherits(token, "character")) {
    stop("Your token must be a character string.")
  }

  # select the token counts from the dfm
  t_counts <- suppressWarnings(
    quanteda::dfm_select(target_dfm, pattern = token, valuetype = "fixed")
  )
  if (length(t_counts) == 0) {
    stop("Token not found in document feature matrix.")
  }
  # convert to a vector
  t_counts <- as.vector(t_counts)
  # calculate the total number of tokens
  total <- sum(suppressWarnings(quanteda::ntoken(target_dfm)))
  # calculate the relative sizes of the parts of the corpus (in percent)
  parts <- suppressWarnings(quanteda::ntoken(target_dfm) / total)

  # The calc_disp1() function is based on a script written by Stefan Th. Gries
  # It computes commonly used measures of dispersion
  # that he discusses in these papers:
  # Gries, Stefan Th. 2008. Dispersions and adjusted frequencies
  # in corpora. /International Journal of Corpus Linguistics/ 13(4). 403-437.
  # Gries, Stefan Th. 2010. Dispersions and adjusted frequencies in corpora:
  # further explorations. In Stefan Th. Gries, Stefanie Wulff, & Mark Davies
  # (eds.), /Corpus linguistic applications: current studies, new directions/,
  # 197-212. Amsterdam: Rodopi.

  calc_disp1 <- function(v, s = rep(1 / length(v))) {

    n <- length(v) # n
    f <- sum(v) # f
    s <- s / sum(s) # s

    values <- list()

    nf <- normalizing_factor(total)

    values[["Absolute frequency"]] <- f
    # note that this is normalizing according to the normalizing factor 'nf'
    values[[names(nf)]] <- (f / total) * as.numeric(nf)
    values[["Relative entropy of all sizes of the corpus parts"]] <-
      -sum(s * log(s)) / log(length(s))

    values[["Range"]] <- sum(v > 0)
    values[["Maxmin"]] <- max(v) - min(v)
    values[["Standard deviation"]] <- sd(v)
    values[["Variation coefficient"]] <- sd(v) / mean(v)
    values[["Chi-square"]] <-
      sum(((v - (f * s / sum(s)))^2) / (f * s / sum(s)))

    values[["Juilland et al.'s D (based on equally-sized corpus parts)"]] <-
      1 - ((sd(v) / mean(v)) / sqrt(n - 1))
    # Juilland measures
    juilland_not_equal_key <- paste0("Juilland et al.'s D (not requiring ",
                                     "equally-sized corpus parts)")
    values[[juilland_not_equal_key]] <-
      1 - ((sd(v / s) / mean(v / s)) / sqrt(length(v / s) - 1))
    values[["Carroll's D2"]] <-
      (log2(f) - (sum(v[v != 0] * log2(v[v != 0])) / f)) / log2(n)
    values[["Rosengren's S (based on equally-sized corpus parts)"]] <-
      ((sum(sqrt(v))^2) / n) / f
    values[["Rosengren's S (not requiring equally-sized corpus parts)"]] <-
      sum(sqrt(v * s))^2 / f
    values[["Lyne's D3 (not requiring equally-sized corpus parts)"]] <-
      1 - ((sum(((v - mean(v))^2) / mean(v))) / (4 * f))
    values[["Distributional consistency DC"]] <-
      ((sum(sqrt(v)) / n)^2) / mean(v)
    values[["Inverse document frequency IDF"]] <- log2(n / sum(v > 0))

    values[["Engvall's measure"]] <- f * (sum(v > 0) / n)
    values[["Juilland et al.'s U (based on equally-sized corpus parts)"]] <-
      f * (1 - ((sd(v) / mean(v)) / sqrt(n - 1)))
    values[["Juilland et al.'U (not requiring equally-sized corpus parts)"]] <-
      f * (1 - ((sd(v / s) / mean(v / s)) / sqrt(length(v / s) - 1)))
    # Carroll's Um calculation broken into parts for readability
    carroll_d2_part <-
      (log2(f) - (sum(v[v != 0] * log2(v[v != 0])) / f)) / log2(n)
    values[["Carroll's Um (based on equally sized corpus parts)"]] <-
      f * carroll_d2_part + (1 - carroll_d2_part) * (f / n)
    # Rosengren measures
    rosengren_equal_key <- paste0("Rosengren's Adjusted Frequency (based on ",
                                  "equally sized corpus parts)")
    values[[rosengren_equal_key]] <-
      (sum(sqrt(v))^2) / n
    rosengren_not_equal_key <- paste0("Rosengren's Adjusted Frequency (not ",
                                      "requiring equally sized corpus parts)")
    values[[rosengren_not_equal_key]] <-
      sum(sqrt(v * s))^2
    values[["Kromer's Ur"]] <- sum(digamma(v + 1) + 0.577215665)

    values[["Deviation of proportions DP"]] <- sum(abs((v / f) - s)) / 2
    values[["Deviation of proportions DP (normalized)"]] <-
      (sum(abs((v / f) - s)) / 2) / (1 - min(s))

    return(values)
  }

  # execute the function and return the values
  dsp <- calc_disp1(t_counts, parts)
  dsp
}

#' Dispersion measures for all tokens in a corpus
#'
#' The dispersions_all() function calculates a subset of of the most common
#' dispersion measures for all of the tokens in a document-feature matrix and
#' returns a data.frame. For example: dispersions_all(my_dfm)
#'
#' @param target_dfm The target document-feature matrix.
#' @return A data.frame containing dispersion measures for the tokens in the
#'   document-feature matrix.
#' @examples
#' \donttest{
#' library(quanteda)
#' texts <- c("The cat sat on the mat",
#'            "The dog ran in the park", 
#'            "The cat and dog are friends")
#' dfm_obj <- dfm(tokens(texts))
#' dispersions <- dispersions_all(dfm_obj)
#' head(dispersions)
#' }
#' @export
dispersions_all <- function(target_dfm) {

  if (!inherits(target_dfm, "dfm")) {
    stop("Your target must be a quanteda dfm object.")
  }
  # convert dfm to matrix object
  m <- as.matrix(target_dfm)
  # create a column index
  idx <- seq_len(ncol(m))
  # calculate the total number of tokens
  total <- sum(rowSums(m))
  # calculate the relative sizes of the parts of the corpus (in percent)
  parts <- rowSums(m) / total
  # The calc_disp() function is based on a script written by Stefan Th. Gries
  # It computes commonly used measures of dispersion that he discusses in these
  # papers:
  # Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora.
  # /International Journal of Corpus Linguistics/ 13(4). 403-437.
  # Gries, Stefan Th. 2010. Dispersions and adjusted frequencies in corpora:
  # further explorations. In Stefan Th. Gries, Stefanie Wulff, & Mark Davies
  # (eds.), /Corpus linguistic applications: current studies, new directions/,
  # 197-212. Amsterdam: Rodopi.
  calc_disp2 <- function(v, s = rep(1 / length(v))) {

    n <- length(v) # n
    f <- sum(v) # f
    s <- s / sum(s) # s

    nf <- normalizing_factor(total)

    values <- list()
    values[["AF"]] <- f
    # note that this is normalizing according to the normalizing factor 'nf'
    values[[names(nf)]] <- (f / total) * as.numeric(nf)
    values[["Carrolls_D2"]] <-
      (log2(f) - (sum(v[v != 0] * log2(v[v != 0])) / f)) / log2(n)
    values[["Rosengrens_S"]] <- sum(sqrt(v * s))^2 / f
    values[["Lynes_D3"]] <-
      1 - ((sum(((v - mean(v))^2) / mean(v))) / (4 * f))
    values[["DC"]] <- ((sum(sqrt(v)) / n)^2) / mean(v)
    values[["Juillands_D"]] <-
      1 - ((sd(v / s) / mean(v / s)) / sqrt(length(v / s) - 1))

    values[["DP"]] <- sum(abs((v / f) - s)) / 2
    values[["DP_norm"]] <-
      (sum(abs((v / f) - s)) / 2) / (1 - min(s))

    values <- as.data.frame(t(as.matrix(unlist(values))))
    return(values)
  }

  # generate a list of dataframes for each token
  dsp <- lapply(idx, function(i) {
    calc_disp2(m[, i], parts)
  })
  # use the rbindlist() function to combine into a single dataframe
  dsp <- data.frame(data.table::rbindlist(dsp))
  # assign the tokens as a column
  dsp$Token <- colnames(m)
  # reorder columns to put Token first
  dsp <- dsp[, c("Token", setdiff(names(dsp), "Token"))]
  dsp <- dsp[order(-dsp$AF), ]
  rownames(dsp) <- seq_len(nrow(dsp))
  dsp
}

#' Average reduced frequencies for all tokens in a corpus
#' ARF calculates average reduced frequency, which combines dispersion and
#' frequency into a single measure. It does this by de-emphasizing occurrences
#' that appear clustered in close proximity.
#'
#' @param target_tkns The target quanteda tokens object.
#' @return A data.frame containing average reduced frequency for each token.
#' @examples
#' \donttest{
#' library(quanteda)
#' texts <- c("The cat sat on the mat",
#'            "The cat ran fast", 
#'            "Another cat story about cats")
#' toks <- tokens(texts)
#' arf_result <- ARF(toks)
#' head(arf_result)
#' }
#' @export
# nolint start: object_name_linter.
ARF <- function(target_tkns) {
  # nolint end

  if (!inherits(target_tkns, "tokens")) {
    stop("ARF requires a quanteda tokens object.")
  }

  # Combine into single tokens vector
  target_tkns <- suppressWarnings(quanteda::tokens_group(
    target_tkns, groups = rep(1, quanteda::ndoc(target_tkns))
  ))

  position_tks <- unlist(unique(target_tkns))
  total_tks <- length(position_tks)

  # Get the indexes of each individual token
  all_positions <- split(seq_along(position_tks), position_tks)

  # Get the index of the first instance of each token
  p <- unlist(lapply(all_positions, `[[`, 1))

  # Calculate ARF
  result <- purrr::map(seq_along(all_positions), function(x) {
    positions <- all_positions[[x]] # Positions of individual token

    v <- total_tks / length(positions) # Calculate chunks
    init_diff <- min(v, min(positions) + (total_tks - max(positions)))
    position_diff <- diff(positions, lag = 1)
    (1 / v) * (init_diff + sum(position_diff[position_diff < v]) +
                 v * (length(positions) -
                        length(position_diff[position_diff < v]) - 1))
  })

  # Construct the data.frame
  df <- data.frame(Token = target_tkns[[1]][p], ARF = sapply(result, "[[", 1))
  df <- df[order(-df$ARF, df$Token), ]
  df
}

#' Descriptive measures for all tokens in a corpus
#' The frequency_table() function aggregates useful descriptive measures:
#' absolute frequency, relative frequency, average reduced frequency, and
#' deviation of proportions.
#'
#' @param target_tkns The target quanteda tokens object.
#' @return A data.frame containing absolute frequency, relative frequency,
#'   average reduced frequency, and deviation of proportions.
#' @examples
#' \donttest{
#' library(quanteda)
#' texts <- c("The cat sat on the mat",
#'            "The dog ran in the park",
#'            "The cat and dog are friends")
#' toks <- tokens(texts)
#' freq_table <- frequency_table(toks)
#' head(freq_table)
#' }
#' @export
frequency_table <- function(target_tkns) {

  if (!inherits(target_tkns, "tokens")) {
    stop("The function requires a quanteda tokens object.")
  }

  arf_df <- ARF(target_tkns)

  target_dfm <- suppressWarnings(quanteda::dfm(target_tkns))
  m <- as.matrix(target_dfm)
  idx <- seq_len(ncol(m))
  total <- sum(rowSums(m))
  # calculate the relative sizes of the parts of the corpus (in percent)
  parts <- rowSums(m) / total

  dp <- function(v, s = rep(1 / length(v))) {

    f <- sum(v) # f
    s <- s / sum(s) # s

    nf <- normalizing_factor(total)

    values <- list()
    values[["AF"]] <- f
    values[[names(nf)]] <- (f / total) * as.numeric(nf)
    values[["DP"]] <- sum(abs((v / f) - s)) / 2
    values <- as.data.frame(t(as.matrix(unlist(values))))
    values
  }

  dsp <- lapply(idx, function(i) {
    dp(m[, i], parts)
  })
  dsp <- data.frame(data.table::rbindlist(dsp))
  dsp$Token <- colnames(m)
  wl <- dplyr::full_join(arf_df, dsp, by = "Token")
  wl <- wl[order(-wl$AF), ]
  wl <- wl[, c(1, 3, 4, 2, 5)]
  wl
}
