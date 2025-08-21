#' Sample text corpus
#'
#' A small corpus compiled following the sampling procedures of the [Corpus of
#' Contemporary American English](https://www.english-corpora.org/coca/).
#' However, it contains only 50 texts from each type, and each text is only
#' about 2,500 words. Thus, it is similar to the [Brown family of
#' corpora](https://www1.essex.ac.uk/linguistics/external/clmt/w3c/corpus_ling/content/corpora/list/private/brown/brown.html)
#' in its size (roughly 1 million words).
#'
#' The corpus contains data from 8 text-types:
#' - Academic
#' - Blog
#' - Fiction
#' - Magazine
#' - News
#' - Spoken
#' - Television & Movie
#' - Web
#'
#' # Warning
#'
#' These data are included only for demonstration purposes. They not compiled to
#' be used for research.
#'
#' @format
#' \describe{
#' \item{doc_id}{Document ID, in the form type_number.}
#' \item{text}{Document text.}
#' }
"sample_corpus"

#' Simplified DocuScope dictionary
#'
#' A quanteda dictionary (see `quanteda::dictionary()`) that allows for token
#' classification via lookups, using `quanteda::tokens_lookup()`. This is a
#' highly simplified version of
#' [DocuScope](https://docuscospacy.readthedocs.io/en/latest/docuscope.html).
#' You can find the [category descriptions
#' here](https://docuscospacy.readthedocs.io/en/latest/docuscope.html#categories).
#'
#' To use the dictionary, fixed matches should be used and tokens should be
#' prepared with minimal processing:
#'
#' ```
#' tokens(some_corpus, remove_punct = FALSE, remove_numbers = FALSE,
#'        remove_symbols = FALSE, what = "word")
#' ```
#'
#' DocuScope follows similar principles to a [Sentiment
#' Lexicon](https://www.saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm) or
#' [LIWC](https://lit.eecs.umich.edu/geoliwc/liwc_dictionary.html) (Linguistic
#' Inquiry and Word Count), but is orders of manitude larger and organizes its
#' categories according to a rhetorical orientation to language.
#'
#' @format quanteda dictionary object (see `quanteda::dictionary()`)
#' @examples
#' library(quanteda)
#'
#' corpus <- sample_corpus$text
#' names(corpus) <- sample_corpus$doc_id
#'
#' toks <- tokens(corpus, remove_punct = FALSE, remove_numbers = FALSE,
#'                remove_symbols = FALSE, what = "word")
#'
#' tokens_lookup(toks, ds_dict, levels = 1, valuetype = "fixed")
"ds_dict"

#' List of common multi-word expressions
#'
#' Multi-word expressions are common English phrases, like "a lot" or
#' "for keeps", that we may want to count as a unit rather than counting the
#' words individually.
#'
#' @format Character vector of multi-word expressions
"multiword_expressions"
