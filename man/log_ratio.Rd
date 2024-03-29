% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/keyness_functions.R
\name{log_ratio}
\alias{log_ratio}
\title{Log-ratio calculation}
\usage{
log_ratio(n_target, n_reference, total_target, total_reference)
}
\arguments{
\item{n_target}{The raw (non-normalized) token count in the target corpus}

\item{n_reference}{The raw (non-normalized) token count in the reference corpus}

\item{total_target}{The total number of tokens in the target corpus}

\item{total_reference}{The total number of tokens in the reference corpus}
}
\value{
A numeric value representing the log ratio
}
\description{
Take a target column and a reference column, and return an effect size
This effect size calculation is called Log Ratio
And was proposed by Andrew Hardie: http://cass.lancs.ac.uk/log-ratio-an-informal-introduction/
}
