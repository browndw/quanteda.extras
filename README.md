# quanteda.extras

[![R-CMD-check](https://github.com/browndw/quanteda.extras/workflows/R-CMD-check/badge.svg)](https://github.com/browndw/quanteda.extras/actions)
[![Tests](https://github.com/browndw/quanteda.extras/workflows/Tests/badge.svg)](https://github.com/browndw/quanteda.extras/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/quanteda.extras)](https://CRAN.R-project.org/package=quanteda.extras)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/quanteda.extras)](https://cran.r-project.org/package=quanteda.extras)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Additional Corpus Functions for Quanteda**

`quanteda.extras` provides statistical and visualization tools that extend the functionality of [quanteda](http://quanteda.io/) for corpus linguistics and text analysis research. The package includes comprehensive functions for:

- **Keyness analysis** - Calculate log-likelihood, effect sizes, and comparative statistics
- **Dispersion measures** - Analyze how words are distributed across texts  
- **Collocational analysis** - Identify and analyze word associations using mutual information
- **Text preprocessing** - Streamlined text cleaning and preparation utilities
- **Data utilities** - Sample corpora and dictionaries for testing and demonstration


## Installation

Install the development version from GitHub:

```r
# Install from GitHub
devtools::install_github("browndw/quanteda.extras")

# Or using pak (recommended)
pak::pak("browndw/quanteda.extras")
```

## Key Features

### 🔑 Keyness Analysis
- `keyness_table()` - Comprehensive keyness statistics with log-likelihood, effect sizes, and p-values
- `key_keys()` - Distribution-aware keyness using Scott's "key of keys" approach  
- `keyness_pairs()` - Pairwise comparisons across multiple corpora
- `log_like()` - Log-likelihood calculations with optional Yates correction
- `log_ratio()` - Hardie's log-ratio effect size calculations

### 📊 Dispersion Measures  
- `dispersions_all()` - Calculate multiple dispersion measures for all tokens
- `dispersions_token()` - Detailed dispersion analysis for individual tokens
- `frequency_table()` - Comprehensive frequency statistics with dispersion measures
- `ARF()` - Average Reduced Frequency combining frequency and dispersion

### 🔗 Collocation Analysis
- `collocates_by_MI()` - Point-wise mutual information calculations (PMI, PMI2, PMI3, NPMI)
- `col_network()` - Network graph preparation for visualization with ggraph

### 🧹 Text Preprocessing
- `preprocess_text()` - Flexible text cleaning with customizable options
- `readtext_lite()` - Lightweight alternative to readtext for simple file reading

### 📚 Sample Data
- `sample_corpus` - Balanced 1M word corpus across 8 text types (Academic, Blog, Fiction, etc.)
- `ds_dict` - Simplified DocuScope dictionary for rhetorical analysis
- `multiword_expressions` - Common English multi-word expressions

## Quick Start

```r
library(quanteda.extras)
library(quanteda)

# Load sample data
data(sample_corpus)

# Basic preprocessing
corpus_clean <- sample_corpus %>%
  mutate(text = preprocess_text(text))

# Create tokens and dfm for keyness analysis
fiction_tokens <- corpus_clean %>%
  filter(grepl("fic", doc_id)) %>%
  corpus() %>%
  tokens(what = "fastestword", remove_numbers = TRUE) %>%
  dfm()

academic_tokens <- corpus_clean %>%
  filter(grepl("aca", doc_id)) %>%
  corpus() %>%
  tokens(what = "fastestword", remove_numbers = TRUE) %>%
  dfm()

# Calculate keyness
keyness_results <- keyness_table(fiction_tokens, academic_tokens)
head(keyness_results)

# Analyze collocations
tokens_all <- corpus_clean %>%
  corpus() %>%
  tokens(what = "fastestword", remove_numbers = TRUE)

collocations <- collocates_by_MI(tokens_all, "very", left = 2, right = 2)
head(collocations)
```

## Documentation

Comprehensive documentation and tutorials are available:

- **Package documentation**: [readthedocs](https://cmu-textstat-docs.readthedocs.io/en/latest/quanteda.extras/quanteda.extras.html)
- **Keyness analysis**: [Tutorial](https://cmu-textstat-docs.readthedocs.io/en/latest/quanteda.extras/vignettes/keyness_introduction.html)
- **Dispersion measures**: [Tutorial](https://cmu-textstat-docs.readthedocs.io/en/latest/quanteda.extras/vignettes/dispersions_introduction.html)  
- **Collocation analysis**: [Tutorial](https://cmu-textstat-docs.readthedocs.io/en/latest/quanteda.extras/vignettes/collocations_introduction.html)
- **Text preprocessing**: [Tutorial](https://cmu-textstat-docs.readthedocs.io/en/latest/quanteda.extras/vignettes/preprocess_introduction.html)

## Function Reference

| Function | Description | Tutorial |
|----------|-------------|----------|
| **Text Preprocessing** | | [📖](https://cmu-textstat-docs.readthedocs.io/en/latest/quanteda.extras/vignettes/preprocess_introduction.html) |
| `preprocess_text()` | Clean text with customizable regex substitutions for contractions, hyphens, etc. | |
| `readtext_lite()` | Lightweight file reading alternative to readtext | |
| **Dispersion Analysis** | | [📖](https://cmu-textstat-docs.readthedocs.io/en/latest/quanteda.extras/vignettes/dispersions_introduction.html) |
| `frequency_table()` | Comprehensive frequency statistics: absolute, relative, ARF, deviation of proportions | |
| `dispersions_all()` | Multiple dispersion measures for all tokens in a corpus | |
| `dispersions_token()` | Detailed dispersion analysis for a single specified token | |
| `ARF()` | Average Reduced Frequency combining frequency and dispersion | |
| **Keyness Analysis** | | [📖](https://cmu-textstat-docs.readthedocs.io/en/latest/quanteda.extras/vignettes/keyness_introduction.html) |
| `keyness_table()` | Complete keyness analysis: log-likelihood, effect sizes, p-values, frequencies | |
| `key_keys()` | Distribution-aware "key of keys" analysis across individual texts | |
| `keyness_pairs()` | Pairwise keyness comparisons across multiple corpora | |
| `log_like()` | Log-likelihood calculations with optional Yates correction | |
| `log_ratio()` | Hardie's log-ratio effect size calculations | |
| **Collocation Analysis** | | [📖](https://cmu-textstat-docs.readthedocs.io/en/latest/quanteda.extras/vignettes/collocations_introduction.html) |
| `collocates_by_MI()` | Point-wise mutual information with multiple measures (PMI, PMI2, PMI3, NPMI) | |
| `col_network()` | Prepare collocation data for network visualization with ggraph | |

## Contributing

We welcome contributions! Please see our [GitHub repository](https://github.com/browndw/quanteda.extras) for:

- Bug reports and feature requests
- Development guidelines  
- Code contributions

## Citation

If you use quanteda.extras in your research, please cite:

```r
citation("quanteda.extras")
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
