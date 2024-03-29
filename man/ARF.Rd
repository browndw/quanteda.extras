% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dispersion_functions.R
\name{ARF}
\alias{ARF}
\title{Average reduced frequencies for all tokens in a corpus}
\usage{
ARF(target_tkns)
}
\arguments{
\item{target_tkns}{The target quanteda tokens object.}
}
\value{
A data.frame containing average reduced frequency for each token.
}
\description{
ARF calculates average reduced frequency, which combines dispersion and frequency into a single measure.
It does this by de-emphasizing occurrences that appear clustered in close proximity.
}
