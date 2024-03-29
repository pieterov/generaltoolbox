% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f - Info.R
\name{f_info}
\alias{f_info}
\title{Print basic statistics on vector or data frame with elements}
\usage{
f_info(x, n.top = 10, show.freq = TRUE, n.width = 29, c.sort.by = "frequency")
}
\arguments{
\item{x}{Vector or data frame with items to check statistics of.}

\item{n.top}{Max number of items to show in the list (default: 10).}

\item{show.freq}{Should frequency be shown? (default: TRUE).}

\item{n.width}{Number of characters to show in the list (default: 29).}

\item{c.sort.by}{How to sort the items in the frequency table, by its 'frequency' or 'value' (default: 'frequency').}
}
\value{
Nothing. Only prints to console.
}
\description{
Print basic statistics on vector of elements or for each feature in a data frame. For each vector
(or feature) the function f_vector_info() is called.
}
\details{
-
}
\examples{
f_info(

      x         = c("A", "B", "C", "A", "B", "A"),
      n.top     = 10,
      show.freq = TRUE,
      c.sort.by = "frequency",
      n.width   = 29
)
}
\author{
Pieter Overdevest
}
