% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/twelve_months_cumulative.R
\name{twelve_months_cumulative}
\alias{twelve_months_cumulative}
\title{Twelve months cumulative variation}
\usage{
twelve_months_cumulative(x, time, percent = F)
}
\arguments{
\item{x}{input data set}

\item{time}{time variable}

\item{percent}{if true, the result is multiplied by 100}
}
\description{
Computes twelve months cumulative variation of a variable.
}
\examples{
library(lubridate)
x <- data.frame( date = seq(as.Date("1910/1/1"), as.Date("1920/1/1"), "months"), x = seq(1:121))
twelve_months_cumulative(x$x, time =  x$date)
}
