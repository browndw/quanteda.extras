## quanteda.extras

[![R-CMD-check](https://github.com/browndw/quanteda.extras/workflows/R-CMD-check/badge.svg)](https://github.com/browndw/quanteda.extras/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/quanteda.extras)](https://CRAN.R-project.org/package=quanteda.extras)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/quanteda.extras)](https://cran.r-project.org/package=quanteda.extras)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

This package contains statstical and plotting tools that expand the functionality of [quanteda](http://quanteda.io/). These include functions for calculating:

* keyness
* effect size
* dispersion
* collocation


## Installing quanteda.extras

Use devtools to install the package.

```r
devtools::install_github("browndw/quanteda.extras")
```

The package documentation is available on [readthedocs](https://cmu-textstat-docs.readthedocs.io/en/latest/quanteda.extras/quanteda.extras.html)

## Using quanteda.extras

The primary functions included in quanteda.extras are listed in the table below, with links to more detailed vignettes.

<table><tbody><tr><td><b>Pre-processing text</b> 
     </td>
<td><a href=https://cmu-textstat-docs.readthedocs.io/en/latest/quanteda.extras/vignettes/preprocess_introduction.html target="_blank">vignette</a></td>
</tr><tr><td>
     <code>preprocess_text()</code> </td>
<td>Requires <b>readtext data.frame</b>. A simple function that requires a readtext object. It then processes the text column using basic regex substitutions. </td>
</tr><td><b>Dispersion</b>
     </td>
<td><a href=https://cmu-textstat-docs.readthedocs.io/en/latest/quanteda.extras/vignettes/dispersions_introduction.html target="_blank">vignette</a></td>
</tr><tr><td>
     <code>frequency_table()</code> </td>
<td>Requires <b>tokens</b>. The function aggregates useful descriptive measures: absolute frequency, relative frequency, average reduced frequency, and deviation of proportions.</em>) </td>
</tr><tr><td>
     <code>dispersions_all()</code> </td>
<td>Requires <b>dfm</b>. The function calculates a subset of of the most common dispersion measures</td>
</tr><tr><td>
     <code>dispersions_token()</code> </td>
<td>Requires <b>dfm</b> and specified token. The function calculates the dispersion measures for a single token.</td>
</tr>
<tr><td>
     <code>ARF()</code> </td>
<td>Requires <b>tokens</b>. The function calculates average reduced frequency, which combines dispersion and frequency into a single measure.</td>
</tr>
<tr><td><b>Keyness</b>
     </td>
<td><a href=https://cmu-textstat-docs.readthedocs.io/en/latest/quanteda.extras/vignettes/keyness_introduction.html target="_blank">vignette</a></td>
</tr><tr><td>
     <code>keyness_table()</code> </td>
<td>Requires target and reference <b>dfms</b>. The function returns the log-likelihood of the target vs. reference corpus, effect sizes by log ratio, <em>p</em>-values, absolute frequencies, relative frequencies, and deviation of proportions.</td>
</tr><tr><td>
     <code>key_keys()</code> </td>
<td>Requires target and reference <b>dfms</b>. The function calculates "key key words" by iterating through each text in the target corpus and calculating keyness values against the reference corpus. Returns a range at which a significance threshold is reached, as well as mean log-likelihood and effect size.</td>
</tr><tr><td>
     <code>keyness_pairs()</code> </td>
<td>Requires 3 or more <b>dfms</b> to compare. The function takes any number of quanteda dfm objects and returns a table of log-likelihood values, effect sizes using Hardie's log ratio and <em>p</em>-values.</td>
</tr><tr><td><b>Collocation</b>
     </td>
<td><a href=https://cmu-textstat-docs.readthedocs.io/en/latest/quanteda.extras/vignettes/collocations_introduction.html target="_blank">vignette</a></td>
</tr><tr><td>
     <code>collocates_by_MI()</code> </td>
<td>Requires <b>tokens</b>. A function for calculating point-wise mutual information from quanteda tokens.</td>
</tr>
<tr><td>
     <code>col_network()</code> </td>
<td>Requires <b>collocation tables</b>. The function takes any number of collocations objects (output from the collocates_by_MI function) and generates a tidygraph data object for plotting collocational networks in ggraph.</td>
</tr>
</tbody></table>
