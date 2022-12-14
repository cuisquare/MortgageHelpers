% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions.R
\name{monthly_interest_paid}
\alias{monthly_interest_paid}
\title{monthly_interest_paid}
\usage{
monthly_interest_paid(rate, capital, rest = 12)
}
\arguments{
\item{rate}{double, the mortgage current rate (absolute value e.g. 1.5\% means rate = 0.015)}

\item{capital}{double, the mortgage remaining capital}

\item{rest}{integer, defaults to 12 for monthly payments}
}
\value{
double Interest paid monthly
}
\description{
Interest paid monthly considering current rate, capital and rest
}
\examples{
monthly_interest_paid(100000,0.015)
}
