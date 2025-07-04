% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pairwise_median_test.R
\name{median_test_pair}
\alias{median_test_pair}
\title{Mood's median tests for one pair of groups}
\usage{
median_test_pair(d, x, g, g1, g2, tol = 1e-06)
}
\arguments{
\item{d}{a data frame}

\item{x}{unquoted name of quantitative variable}

\item{g}{unquoted name of grouping variable (as character, not a factor)}

\item{g1}{first group to compare (as text)}

\item{g2}{second group to compare (as text)}

\item{tol}{(default 1e-6) any data values closer to overall median than this are discarded}
}
\value{
(two-sided) P-value
}
\description{
Mood's median tests for one pair of groups
}
\examples{
median_test_pair(datasets::mtcars, mpg, cyl, 4, 8)

}
\author{
Ken Butler, \email{butler@utsc.utoronto.ca}
}
