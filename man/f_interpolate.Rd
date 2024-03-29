% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f - Interpolate.R
\name{f_interpolate}
\alias{f_interpolate}
\title{Interpolate between points}
\usage{
f_interpolate(
  df.input,
  v.coord.point = c("point.x", "point.y"),
  n.threshold.distance
)
}
\arguments{
\item{df.input}{-----}

\item{v.coord.point}{Vector with names of x- and y-coordinate (default: c("point.x", "point.y")).}

\item{n.threshold.distance}{Distance between interpolated points will not exceed n.threshold.distance.}
}
\value{
Data frame with interpolated coordinates.
}
\description{
Interpolate between points when their distance exceeds threshold.
}
\details{
-
}
\examples{
df.output <- f_interpolate(

   df.input             = tibble(

        point.x = c(110000, 130000, 150000, 130000, 110000),

        point.y = c(400000, 430000, 400000, 370000, 400000)
   ),

   v.coord.point        = c("point.x", "point.y"),

   n.threshold.distance = 500
)
}
\author{
Pieter Overdevest
}
