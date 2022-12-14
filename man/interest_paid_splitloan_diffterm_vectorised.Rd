% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions.R
\name{interest_paid_splitloan_diffterm_vectorised}
\alias{interest_paid_splitloan_diffterm_vectorised}
\title{interest_paid_splitloan_diffterm_vectorised}
\usage{
interest_paid_splitloan_diffterm_vectorised(
  rate = rep(0.0154, month_num),
  capital1,
  capital2,
  term_months1,
  term_months2,
  month_num,
  month_overpay = rep(0, month_num),
  perc_overpay1 = rep(0, month_num),
  keep_mp_base = TRUE
)
}
\arguments{
\item{rate}{vector double of length month_num, the mortgage current rate (absolute value e.g. 1.5\% means rate = 0.015)}

\item{capital1}{double, the mortgage remaining capital1}

\item{capital2}{double, the mortgage remaining capital2}

\item{term_months1}{integer, number of months remaining to pay the 1st mortgage back}

\item{term_months2}{integer, number of months remaining to pay the 2nd mortgage back}

\item{month_num}{integer, duration of payment period, in months, payments equal those required to pay off mortgage without overpayments}

\item{month_overpay}{vector integer of length month_num, amount overpaid each month, defaults to 0}

\item{perc_overpay1}{vector double of length month_num, percentage of overpay dedicated to first loan}

\item{keep_mp_base}{boolean, if TRUE (default) the based payments of a paid off loadn will be used
towards the remaining loan.}
}
\value{
named list reflecting state of both mortgage, with members capital1, capital2, act_month1, act_month2, int_paid1, int_paid2, int_paid
}
\description{
interest paid on two loans of same rate but different terms.
Vectorised, that is all input are vectors so all relevant values
(rate, month_overpay and perc_overpay1)
can be varied through the term.
}
