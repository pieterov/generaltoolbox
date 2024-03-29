% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f - Clean up header names.R
\name{f_clean_up_header_names}
\alias{f_clean_up_header_names}
\title{Clean up header names}
\usage{
f_clean_up_header_names(df.data)
}
\arguments{
\item{df.data}{Data frame whose names need to be cleaned.}
}
\value{
Data frame with cleaned up names.
}
\description{
Clean up header names.
}
\details{
Special characters are replaced by period.
}
\examples{
df.data <- f_clean_up_header_names(

    df.data
)
}
\author{
Pieter Overdevest
}
