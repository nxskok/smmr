% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pval_sign.R
\name{pval_sign}
\alias{pval_sign}
\title{Two-sided P-value for sign test}
\usage{
pval_sign(med0, d, x)
}
\arguments{
\item{med0}{null median}

\item{d}{data frame}

\item{x}{vector of data for test}
}
\value{
P-value of two-sided sign test for median
}
\description{
Two-sided P-value for sign test
}
\examples{
d <- data.frame(z = 1:10)
pval_sign(3.5, d, z)
pval_sign(3, d, z)
pval_sign(25, datasets::mtcars, mpg)

}
\author{
Ken Butler, \email{butler@utsc.utoronto.ca}
}
