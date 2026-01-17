#' @title monthly_interest_paid
#'
#' @description Interest paid monthly considering current rate, capital and rest
#'
#' @param rate double, the mortgage current rate (absolute value e.g. 1.5\% means rate = 0.015)
#' @param capital double, the mortgage remaining capital
#' @param rest integer, defaults to 12 for monthly payments
#'
#' @return double Interest paid monthly
#' @export
#'
#' @examples monthly_interest_paid(100000,0.015)
monthly_interest_paid <- function(rate,capital,rest=12) {
  capital*rate/rest
}

#' @title monthly_payment
#'
#' @description monthly payment for a classic mortgage
#'
#' @param rate double, the mortgage current rate (absolute value e.g. 1.5\% means rate = 0.015)
#' @param capital double, the mortgage remaining capital
#' @param term_months integer, number of months remaining to pay the mortgage back integer, number of months remaining to pay the mortgage back
#' @param rest integer, defaults to 12 for monthly payments
#'
#' @return double monthly payment
#' @export
#'
monthly_payment <- function(rate, capital, term_months,rest=12) {
  mr <- rate /rest
  capital * (mr * (1+mr)^(term_months))/((1+mr)^(term_months)-1)
}

#' Implied remaining mortgage term assuming constant base payment
#'
#' Computes the implied remaining term (in months) for a mortgage given
#' the current outstanding capital, a constant monthly payment, and
#' a fixed annual interest rate.
#'
#' This represents the number of months it would take to fully repay the
#' remaining capital **if no further overpayments are made** and the base
#' payment is kept constant.
#'
#' @param rate Numeric scalar. Annual interest rate (e.g. \code{0.039} for 3.9\%).
#' @param capital Numeric scalar. Remaining mortgage capital after a payment.
#' @param base_payment Numeric scalar. Monthly payment assumed going forward
#'   (typically the contractual payment excluding overpayments).
#' @param rest integer, defaults to 12 for monthly payments
#'
#' @details
#' If \code{base_payment <= capital * rate / 12}, the mortgage does
#' not amortise and the implied remaining term is infinite.
#'
#' @return Numeric scalar. Remaining term in months. May be \code{Inf}.
#'
#' @examples
#' implied_term_months(
#'   capital = 150000,
#'   base_payment = 900,
#'   rate = 0.04
#' )
#'
#' @export
implied_term_months <- function(rate,capital, base_payment,rest = 12) {
  mr <- rate / rest

  if (is.na(capital) || is.na(base_payment) || is.na(mr)) return(NA_real_)
  if (capital <= 0) return(0)
  if (base_payment <= 0) return(Inf)

  # Zero-interest case
  if (mr == 0) return(capital / base_payment)

  # Not covering interest -> negative amortisation
  if (base_payment <= mr * capital) return(Inf)

  log(base_payment / (base_payment - mr * capital)) / log1p(mr)
}

#' @title capital_remaining
#'
#' @description capital remaining after paying for month_num months
#'
#' @param rate double, the mortgage current rate (absolute value e.g. 1.5\% means rate = 0.015)
#' @param capital double, the mortgage remaining capital
#' @param term_months integer, number of months remaining to pay the mortgage back
#' @param month_num integer, duration of payment period, in months, payments equal those required to pay off mortgage without overpayments
#' @param month_overpay integer, amount overpaid each month, defaults to 0
#'
#' @return double capital remaining after month_num
#' @export
#'
capital_remaining <- function(rate,capital,term_months,month_num,month_overpay = 0) {
  m_p <- monthly_payment(rate, capital, term_months) + month_overpay
  for (month in 1:month_num) {
    m_i_p <- monthly_interest_paid(rate,capital)
    c_p <- min(m_p - m_i_p, capital)
    capital <- capital - c_p
    if (capital == 0) {
      warning("capital reduced to zero")
      break
    }
  }
  return(capital)
}

#' @title interest_paid
#'
#' @param rate double, the mortgage current rate (absolute value e.g. 1.5\% means rate = 0.015)
#' @param capital double, the mortgage remaining capital
#' @param term_months integer, number of months remaining to pay the mortgage back
#' @param month_num integer, duration of payment period, in months, payments equal those required to pay off mortgage without overpayments
#' @param month_overpay integer, amount overpaid each month, defaults to 0
#'
#' @return double interest paid over the mortgage duration
#' @export
#'
interest_paid <- function(rate,capital,term_months,month_num,month_overpay = 0) {
  m_p <- monthly_payment(rate, capital, term_months) + month_overpay
  m_i_p_tot <- 0
  for (month in 1:month_num) {
    m_i_p <- monthly_interest_paid(rate,capital)
    m_i_p_tot <- m_i_p_tot + m_i_p
    c_p <- min(m_p - m_i_p, capital)
    capital <- capital - c_p
    if (capital == 0) {
      warning(paste("capital reduced to zero at month number", month ))
      break
    }
  }
  return(list(act_month = month,int_paid = m_i_p_tot))
}


#' @title interest_paid_splitloan
#'
#' @description interest paid if the loan is split in two with same rate.
#'
#' @param rate double, the mortgage current rate (absolute value e.g. 1.5\% means rate = 0.015)
#' @param capital1 double, the mortgage remaining capital1
#' @param capital2 double, the mortgage remaining capital2
#' @param term_months integer, number of months remaining to pay the mortgage back
#' @param month_num integer, duration of payment period, in months, payments equal those required to pay off mortgage without overpayments
#' @param month_overpay integer, amount overpaid each month, defaults to 0
#' @param perc_overpay1 percentage of overpay dedicated to first loan
#' @param perc_overpay2 percentage of overpay dedicated to second loan
#'
#' @return double interest paid
#' @export
#'
interest_paid_splitloan <-  function(rate,
                                     capital1,
                                     capital2,
                                     term_months,
                                     month_num,
                                     month_overpay = 0,
                                     perc_overpay1,
                                     perc_overpay2) {

  m_p <- monthly_payment(rate, capital1 + capital2, term_months)

  m_p1_base <- m_p * capital1 / (capital1 + capital2)
  m_p1 <- m_p1_base + perc_overpay1 * month_overpay

  m_p2_base <- m_p * capital2 / (capital1 + capital2)
  m_p2 <- m_p2_base + perc_overpay2 * month_overpay

  m_i_p_tot1 <- 0
  m_i_p_tot2 <- 0
  for (month in 1:month_num) {
    if (capital1 != 0) {
      m_i_p1 <- monthly_interest_paid(rate, capital1)
      m_i_p_tot1 <- m_i_p_tot1 + m_i_p1
      c_p1 <- min(m_p1 - m_i_p1, capital1)
      capital1 <- capital1 - c_p1
      if (capital1 == 0) {
        warning(paste("capital1 reduced to zero at month number", month))
        perc_overpay1 <- 0
        perc_overpay2 <- 1
        m_p2_base <- m_p * capital2 / (capital1 + capital2)
        m_p2 <- m_p2_base + perc_overpay2 * month_overpay
      }
    }

    if (capital2 != 0) {
      m_i_p2 <- monthly_interest_paid(rate, capital2)
      m_i_p_tot2 <- m_i_p_tot2 + m_i_p2
      c_p2 <- min(m_p2 - m_i_p2, capital2)
      capital2 <- capital2 - c_p2
      if (capital2 == 0) {
        warning(paste("capital2 reduced to zero at month number", month))
        perc_overpay2 <- 0
        perc_overpay1 <- 1
        m_p1_base <- m_p * capital1 / (capital1 + capital2)
        m_p1 <- m_p1_base + perc_overpay1 * month_overpay
      }
    }

    if (capital1 + capital2 == 0) {
      warning("both capital reduced to 0 before number of months requested")
      break
    }
  }
  return(list(capital1 = capital1, capital2 = capital2, act_month = month, int_paid1 = m_i_p_tot1, int_paid2 = m_i_p_tot2))
}

#' @title interest_paid_splitloan_diffterm
#'
#' @description interest paid on two loans of same rate but different terms
#'
#' @param rate double, the mortgage current rate (absolute value e.g. 1.5\% means rate = 0.015)
#' @param capital1 double, the mortgage remaining capital1
#' @param capital2 double, the mortgage remaining capital2
#' @param term_months1 integer, number of months remaining to pay the 1st mortgage back
#' @param term_months2 integer, number of months remaining to pay the 2nd mortgage back
#' @param month_num integer, duration of payment period, in months, payments equal those required to pay off mortgage without overpayments
#' @param month_overpay integer, amount overpaid each month, defaults to 0
#' @param perc_overpay1 percentage of overpay dedicated to first loan
#'
#' @return named list reflecting state of both mortgage, with members capital1, capital2, act_month1, act_month2, int_paid1, int_paid2, int_paid
#' @export
#'
interest_paid_splitloan_diffterm <-  function(rate,
                                     capital1,
                                     capital2,
                                     term_months1,
                                     term_months2,
                                     month_num,
                                     month_overpay = rep(0,month_num),
                                     perc_overpay1) {

  if (length(month_overpay) == 1) {
    month_overpay <- rep(month_overpay,month_num)
  }

  perc_overpay2 <- 1 - perc_overpay1

  m_p1_base <- monthly_payment(rate, capital1, term_months1)

  m_p2_base <- monthly_payment(rate, capital2, term_months2)

  m_i_p_tot1 <- 0
  m_i_p_tot2 <- 0
  for (month in 1:month_num) {
    #print(paste("month = ",month," and month_overpay[month] = ", month_overpay[month]))
    if (capital1 != 0) {
      #print(paste("in part 1, month = ",month," and month_overpay[month] = ", month_overpay[month]))
      #monthly payment
      m_p1 <- m_p1_base + perc_overpay1 * month_overpay[month]
      #interest paid over the month
      m_i_p1 <- monthly_interest_paid(rate, capital1)
      #total interest paid from the start
      m_i_p_tot1 <- m_i_p_tot1 + m_i_p1
      #capital paid over the month is either the remaining capital or monthly payment after removing interest
      c_p1 <- min(m_p1 - m_i_p1, capital1)
      #new capital at end of month
      capital1 <- capital1 - c_p1
      if (capital1 == 0) {
        warning(paste("capital1 reduced to zero at month number", month))
        #since the first bit is paid off, we diverge 100\% of overpayment to 2nd loan
        perc_overpay1 <- 0
        perc_overpay2 <- 1
        # m_p2 <- monthly_payment(rate, capital2, term_months2) + perc_overpay2 * month_overpay[month]
      }
      act_month1 <- month
    }


    if (capital2 != 0) {
      #print(paste("in part 2, month = ",month," and month_overpay[month] = ", month_overpay[month]))
      m_p2 <- m_p2_base + perc_overpay2 * month_overpay[month]
      m_i_p2 <- monthly_interest_paid(rate, capital2)
      m_i_p_tot2 <- m_i_p_tot2 + m_i_p2
      c_p2 <- min(m_p2 - m_i_p2, capital2)
      capital2 <- capital2 - c_p2
      if (capital2 == 0) {
        warning(paste("capital2 reduced to zero at month number", month))
        perc_overpay2 <- 0
        perc_overpay1 <- 1
        # m_p1 <- monthly_payment(rate, capital1, term_months1) + perc_overpay1 * month_overpay[month]
      }
      act_month2 <- month
    }


    if (capital1 + capital2 == 0) {
      warning("both capital reduced to 0 before number of months requested")
      break
    }
  }
  return(list(capital1 = capital1,
              capital2 = capital2,
              act_month = month,
              act_month1 = act_month1,
              act_month2 = act_month2,
              int_paid1 = m_i_p_tot1,
              int_paid2 = m_i_p_tot2,
              int_paid = m_i_p_tot1 + m_i_p_tot2))
}


#' @title interest_paid_splitloan_diffterm_vectorised
#'
#' @description interest paid on two loans of same rate but different terms.
#' Vectorised, that is all input are vectors so all relevant values
#' (rate, month_overpay and perc_overpay1)
#' can be varied through the term.
#'
#' @param rate vector double of length month_num, the mortgage current rate (absolute value e.g. 1.5\% means rate = 0.015)
#' @param capital1 double, the mortgage remaining capital1
#' @param capital2 double, the mortgage remaining capital2
#' @param term_months1 integer, number of months remaining to pay the 1st mortgage back
#' @param term_months2 integer, number of months remaining to pay the 2nd mortgage back
#' @param month_num integer, duration of payment period, in months, payments equal those required to pay off mortgage without overpayments
#' @param month_overpay vector integer of length month_num, amount overpaid each month, defaults to 0
#' @param perc_overpay1 vector double of length month_num, percentage of overpay dedicated to first loan
#' @param keep_mp_base boolean, if TRUE (default) the based payments of a paid off loadn will be used
#' towards the remaining loan.
#'
#' @return named list reflecting state of both mortgage, with members capital1, capital2, act_month1, act_month2, int_paid1, int_paid2, int_paid
#' @export
#'
interest_paid_splitloan_diffterm_vectorised <-  function(rate = rep(0.0154,month_num),
                                                         capital1,
                                                         capital2,
                                                         term_months1,
                                                         term_months2,
                                                         month_num,
                                                         month_overpay = rep(0,month_num),
                                                         perc_overpay1 = rep(0,month_num),
                                                         keep_mp_base = TRUE) {

  if (length(rate) == 1) {
    rate <- rep(rate,month_num)
  }

  if (length(month_overpay) == 1) {
    month_overpay <- rep(month_overpay,month_num)
  }

  if (length(perc_overpay1) == 1) {
    perc_overpay1 <- rep(perc_overpay1,month_num)
  }

  perc_overpay2 <- 1 - perc_overpay1


  tot_overpay1 <- 0
  tot_overpay2 <- 0

  m_i_p_tot1 <- 0
  m_i_p_tot2 <- 0
  for (month in 1:month_num) {
    #base monthly payment set at first month or when rate changes
    if (month > 1)
    {
      reset_mp_base <- (rate[month] != rate[month-1])
    } else {
      reset_mp_base <- TRUE
    }
    #reset_mp_base <- TRUE # for testing
    if (reset_mp_base) {
      m_p1_base <- monthly_payment(rate[month], capital1, term_months1-month+1)

      m_p2_base <- monthly_payment(rate[month], capital2, term_months2-month+1)

      # print(paste0("At month ",month))
      # print(paste0("m_p1_base: ",m_p1_base))
      # print(paste0("m_p2_base: ",m_p2_base))
    }
    #print(paste("month = ",month," and month_overpay[month] = ", month_overpay[month]))
    if (capital1 != 0) {
      #print(paste("in part 1, month = ",month," and month_overpay[month] = ", month_overpay[month]))
      #monthly payment
      m_p1 <- m_p1_base + perc_overpay1[month] * month_overpay[month]
      #interest paid over the month
      m_i_p1 <- monthly_interest_paid(rate[month], capital1)
      #total interest paid from the start
      m_i_p_tot1 <- m_i_p_tot1 + m_i_p1
      #capital paid over the month is either the remaining capital or monthly payment after removing interest
      c_p1 <- min(m_p1 - m_i_p1, capital1)
      #new capital at end of month
      capital1 <- capital1 - c_p1
      if (capital1 == 0) {
        warning(paste("capital1 reduced to zero at month number", month))
        #since the first bit is paid off, we diverge 100\% of overpayment to 2nd loan
        perc_overpay1[month+1:length(perc_overpay1)] <- 0
        perc_overpay2[month+1:length(perc_overpay2)] <- 1
        #we suppose that the whole base payment is reversed onto other part of loan going forward.
        if (keep_mp_base) {
          m_p2_base <- m_p2_base + m_p1_base
        }
        # m_p2 <- monthly_payment(rate, capital2, term_months2) + perc_overpay2 * month_overpay[month]
      }
      #total overpayment
      inc_overpay1 <- max(m_p1 - m_p1_base,0)
      tot_overpay1 <- tot_overpay1 + inc_overpay1
      # if(inc_overpay1 > 0) {
      #   print(paste0("perc_overpay1 = ",perc_overpay1[month]))
      #   print(paste0("inc_overpay1 = ",inc_overpay1))
      #   print(paste0("tot_overpay1 = ",tot_overpay1))
      #   }
      act_month1 <- month
    }


    if (capital2 != 0) {
      #print(paste("in part 2, month = ",month," and month_overpay[month] = ", month_overpay[month]))
      m_p2 <- m_p2_base + perc_overpay2[month] * month_overpay[month]
      m_i_p2 <- monthly_interest_paid(rate[month], capital2)
      m_i_p_tot2 <- m_i_p_tot2 + m_i_p2
      c_p2 <- min(m_p2 - m_i_p2, capital2)
      capital2 <- capital2 - c_p2
      if (capital2 == 0) {
        warning(paste("capital2 reduced to zero at month number", month))
        perc_overpay2[month+1:length(perc_overpay2)] <- 0
        perc_overpay1[month+1:length(perc_overpay1)] <- 1
        if (keep_mp_base) {
          m_p1_base <- m_p1_base + m_p2_base
        }
        # m_p1 <- monthly_payment(rate, capital1, term_months1) + perc_overpay1 * month_overpay[month]
      }
      #total overpayment
      tot_overpay2 <- tot_overpay2 + max(m_p2 - m_p2_base,0)
      act_month2 <- month
    }


    if (capital1 + capital2 == 0) {
      warning("both capital reduced to 0 before number of months requested")
      break
    }
  }
  return(list(capital1 = capital1,
              capital2 = capital2,
              capital = capital1 + capital2,
              act_month = month,
              act_month1 = act_month1,
              act_month2 = act_month2,
              int_paid1 = m_i_p_tot1,
              int_paid2 = m_i_p_tot2,
              int_paid = m_i_p_tot1 + m_i_p_tot2,
              tot_overpay1 = tot_overpay1,
              tot_overpay2 = tot_overpay2,
              tot_overpay = tot_overpay1+ tot_overpay2,
              keep_mp_base = keep_mp_base))
}

#' Create a mortgage product definition
#'
#' Helper function to define a single mortgage product with linked parameters
#' (e.g. interest rate and fees). Intended to be combined row-by-row using
#' \code{dplyr::bind_rows()} when building a product set for comparison.
#'
#' @param name Character scalar. Name of the mortgage product.
#' @param rate Numeric scalar. Annual interest rate (e.g. 0.0365 for 3.65%).
#' @param fees Numeric scalar. Product fees in currency units (e.g. GBP).
#' @param fixed_term_years Numeric scalar. Length of the fixed-rate period in years.
#'
#' @return A one-row \code{data.frame} describing the mortgage product.
#'
#' @examples
#' products <- dplyr::bind_rows(
#'   mortgage_product(
#'     "NATWEST_2yr_fix_fees",
#'     rate = 0.0364,
#'     fees = 1025,
#'     fixed_term_years = 2
#'   ),
#'   mortgage_product(
#'     "NATWEST_2yr_fix_lowfees",
#'     rate = 0.0398,
#'     fees = 30,
#'     fixed_term_years = 2
#'   ),
#'   mortgage_product(
#'     "BARCLAYS_2yr_fix_lowfees",
#'     rate = 0.0400,
#'     fees = 35,
#'     fixed_term_years = 2
#'   ),
#'   mortgage_product(
#'     "SANTANDER_2yr_fix_fees",
#'     rate = 0.0368,
#'     fees = 999,
#'     fixed_term_years = 2
#'   ),
#'   mortgage_product(
#'     "SANTANDER_2yr_fix_nofees",
#'     rate = 0.0396,
#'     fees = 0,
#'     fixed_term_years = 2
#'   ),
#'   mortgage_product(
#'     "HSBC_2yr_fix_nofees",
#'     rate = 0.0399,
#'     fees = 0,
#'     fixed_term_years = 2
#'   )
#' )
#'
#' products
#'
#' @export
mortgage_product <- function(name, rate, fees, fixed_term_years) {
  data.frame(
    product_name = name,
    rate = rate,
    fees = fees,
    fixed_term_years = fixed_term_years,
    stringsAsFactors = FALSE
  )
}

#' @title mortgage_savings_fees_comparison_grid
#'
#' @description gives summary of mortgage costs considering parameters, as a dataframe
#'
#' @param rate vector double, the mortgage rates (absolute value e.g. 1.5\% means rate = 0.015)
#' @param fees vector double, the mortgage fees
#' @param add_fees_to_loan vector boolean, whether to add the fees to the loan (TRUE) or pay it off as a single down payment (FALSE)
#' @param capital vector double, the mortgage remaining capital
#' @param fixed_term_years vector integer, number of years fixed term assured
#' @param term_months vector integer, number of months remaining to pay the mortgage back without overpayments
#' @param savings integer, amount in savings prior to duration
#' @param flat_value integer, value of the flat
#' @param down_payment vector integer, amount to take from savings and overpay ahead of loan
#' @param month_num integer, duration of payment period considered, in months
#' @param month_overpay_vect list vector double, amount overpaid each month, defaults to rep(0,month_num)
#' @param reduce_term vector boolean, defaults to TRUE do overpayments reduce terms
#' @param case_name_fun optional function to derive the case name, defaults to a concatenation of relevant parameters
#' @param normalize_cashflow optional boolean whether to normalize the cashflow
#' @param max_monthly_outgoing optional double maximum amount payable per month, cases greater than the value will be removed with a warning
#'max_monthly_outgoing
#' @param products optional data.frame bundling linked product params with the required columns being (product_name, rate, fees, fixed_term_years). if provided, rate, fees from the parameters will be ignored.
#'
#' @return  dataframe with each observation being case and
#' variable including case_name, rate, capital_start, reduce_term, monthly_payment_tot, interest_paid_tot and capital_end
#' @export
#'
mortgage_savings_fees_comparison_grid <- function(
    rate,
    fees,
    add_fees_to_loan,
    capital,
    fixed_term_years = NA,
    term_months,
    savings,
    flat_value,
    down_payment,
    month_num,
    month_overpay_vect,          # list of numeric vectors length month_num
    reduce_term = TRUE,
    case_name_fun = NULL,        # optional function(one_row_df) -> string
    normalize_cashflow = TRUE,
    max_monthly_outgoing = Inf,  # cap on any month's mortgage outgoing (base + overpay)
    products = NULL              # data.frame bundling linked product params (rate, fees)
) {
  # ---- validate overpay list ----
  if (!is.list(month_overpay_vect) || length(month_overpay_vect) < 1) {
    stop("`month_overpay_vect` must be a non-empty list of numeric vectors.", call. = FALSE)
  }
  ok <- vapply(month_overpay_vect, function(v) is.numeric(v) && length(v) == month_num, logical(1))
  if (!all(ok)) {
    bad <- which(!ok)
    stop(
      sprintf(
        "All elements of `month_overpay_vect` must be numeric vectors of length `month_num` (%d). Bad indices: %s",
        month_num, paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # ---- products mode validation / normalization ----
  if (!is.null(products)) {
    if (!is.data.frame(products)) {
      stop("`products` must be a data.frame (or tibble).", call. = FALSE)
    }
    required_cols <- c("rate", "fees")
    missing_cols <- setdiff(required_cols, names(products))
    if (length(missing_cols) > 0) {
      stop(sprintf("`products` is missing required column(s): %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
    }

    if (!("product_name" %in% names(products))) {
      products$product_name <- paste0("product", seq_len(nrow(products)))
    } else {
      products$product_name <- as.character(products$product_name)
    }

    # Guard against ambiguous double-specification
    if (length(rate) > 1 || length(fees) > 1) {
      stop("When `products` is provided, do not pass vector `rate`/`fees`. Put them in `products`.", call. = FALSE)
    }
  }

  # ---- build grid ----
  choices_grid <- expand.grid(
    add_fees_to_loan = add_fees_to_loan,
    capital = capital,
    term_months = term_months,
    down_payment = down_payment,
    reduce_term = reduce_term,
    overpay_id = seq_along(month_overpay_vect),
    stringsAsFactors = FALSE
  )

  if (!is.null(products)) {
    grid <- merge(products, choices_grid, by = NULL)  # cartesian product
  } else {
    grid <- merge(
      expand.grid(rate = rate, fees = fees, stringsAsFactors = FALSE),
      choices_grid,
      by = NULL
    )
    grid$product_name <- NA_character_
  }

  # ---- fees==0: remove fee-financing dimension (avoid duplicates) ----
  # If fees are zero, financing/upfront is meaningless. Force a single value and drop duplicates.
  grid$add_fees_to_loan <- ifelse(grid$fees == 0, FALSE, grid$add_fees_to_loan)
  grid <- unique(grid)

  # ---- remove impossible cases: negative savings after upfront actions ----
  upfront_fee_paid_grid <- ifelse(grid$add_fees_to_loan, 0, grid$fees)
  savings_after_grid <- savings - grid$down_payment - upfront_fee_paid_grid

  impossible <- savings_after_grid < 0
  if (any(impossible)) {
    warning(
      sprintf(
        "Removed %d impossible case(s): down_payment + upfront fees exceed available savings.",
        sum(impossible)
      ),
      call. = FALSE
    )
    grid <- grid[!impossible, , drop = FALSE]
  }
  if (nrow(grid) == 0) {
    stop("All grid cases are impossible: down_payment + upfront fees exceed available savings.", call. = FALSE)
  }

  # ---- default case naming helpers ----
  GBP_escaped <- "\u00A3"
  # fmt_gbp <- function(x) {
  #   x <- round(as.numeric(x), 2)
  #   if (is.na(x)) return(paste0(GBP_escaped, "NA"))
  #   if (abs(x - round(x)) < 1e-9) {
  #     paste0(GBP_escaped, format(round(x), big.mark = ",", scientific = FALSE))
  #   } else {
  #     paste0(GBP_escaped, format(x, nsmall = 2, big.mark = ",", scientific = FALSE))
  #   }
  # }

  fmt_gbp <- function(x) {
    x <- round(as.numeric(x), 2)

    out <- rep(paste0(GBP_escaped, "NA"), length(x))

    ok <- !is.na(x)
    if (!any(ok)) return(out)

    x_ok <- x[ok]
    is_int <- abs(x_ok - round(x_ok)) < 1e-9

    out_ok <- character(length(x_ok))
    out_ok[is_int] <- paste0(
      GBP_escaped,
      format(round(x_ok[is_int]), big.mark = ",", scientific = FALSE)
    )
    out_ok[!is_int] <- paste0(
      GBP_escaped,
      format(x_ok[!is_int], nsmall = 2, big.mark = ",", scientific = FALSE)
    )

    out[ok] <- out_ok
    out
  }

  # Create a single naming token that combines fee amount + financing choice
  grid$fee_policy <- ifelse(
    grid$fees == 0,
    "fees=none",
    paste0("fees=", fmt_gbp(grid$fees), ifelse(grid$add_fees_to_loan, " financed", " upfront"))
  )

  # ---- decide which columns appear in the name ----
  varying_cols <- vapply(grid, function(x) length(unique(x)) > 1, logical(1))

  # Remove the old separate fee columns from naming; use fee_policy instead
  varying_cols["fees"] <- FALSE
  varying_cols["add_fees_to_loan"] <- FALSE
  varying_cols["fee_policy"] <- length(unique(grid$fee_policy)) > 1

  format_value <- function(name, value) {

    overpay_desc <- function(overpay_id) {
      v <- month_overpay_vect[[overpay_id]]
      if (!is.numeric(v) || length(v) != month_num) return(paste0("overpay#", overpay_id))

      v <- round(v, 2)
      v[abs(v) < 1e-9] <- 0
      if (all(v == 0)) return("no overpayment")

      r <- rle(v)
      vals <- r$values
      lens <- r$lengths
      ends <- cumsum(lens)
      starts <- ends - lens + 1

      if (length(vals) == 1 && vals[1] != 0) {
        return(paste0(fmt_gbp(vals[1]), " every month"))
      }

      nz_months <- which(v != 0)
      if (length(nz_months) == 1) {
        m <- nz_months[1]
        return(paste0(fmt_gbp(v[m]), " on month ", m))
      }

      parts <- character(0)
      for (k in seq_along(vals)) {
        if (vals[k] == 0) next
        s <- starts[k]
        e <- ends[k]
        amount <- fmt_gbp(vals[k])

        if (s == e) {
          parts <- c(parts, paste0(amount, " on month ", s))
          next
        }
        if (e == month_num) {
          if (s == 1) parts <- c(parts, paste0(amount, " every month"))
          else parts <- c(parts, paste0(amount, " every month from month ", s))
          next
        }
        parts <- c(parts, paste0(amount, " on months ", s, "-", e))
      }
      paste(parts, collapse = " then ")
    }

    switch(
      name,
      product_name = paste0("product=", value),
      rate = paste0("rate=", round(100 * value, 3), "%"),
      fee_policy = as.character(value),
      capital = paste0("cap=", value),
      term_months = paste0("term=", value),
      down_payment = paste0("dp=", value),
      reduce_term = paste0("reduce_term=", value),
      overpay_id = overpay_desc(value),
      paste0(name, "=", value)
    )
  }

  if (is.null(case_name_fun)) {
    cols_to_use <- names(varying_cols)[varying_cols]
    grid$case_name <- vapply(seq_len(nrow(grid)), function(i) {
      parts <- vapply(cols_to_use, function(col) format_value(col, grid[[col]][i]), character(1))
      paste(parts, collapse = " | ")
    }, character(1))
  } else {
    grid$case_name <- vapply(seq_len(nrow(grid)), function(i) {
      case_name_fun(grid[i, , drop = FALSE])
    }, character(1))
  }

  # ---- run ----
  rows <- vector("list", nrow(grid))
  removed_cap <- 0L

  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]

    overpay_sum <- sum(month_overpay_vect[[g$overpay_id]], na.rm = TRUE)
    total_overpayment <- g$down_payment + overpay_sum

    upfront_fee_paid <- if (isTRUE(g$add_fees_to_loan)) 0 else g$fees
    savings_after <- savings - g$down_payment - upfront_fee_paid

    capital_increase_fees = if (isTRUE(g$add_fees_to_loan)) g$fees else 0
    capital_start_horizon <- g$capital - g$down_payment + capital_increase_fees

    if (capital_start_horizon < 0) {
      stop(sprintf("Case %d: down_payment exceeds capital.", i), call. = FALSE)
    }

    total_value_start <- flat_value + savings_after - capital_start_horizon

    ms <- mortgage_state_allmonths_vectorised(
      rate = g$rate,
      capital = capital_start_horizon,
      term_months = g$term_months,
      month_num = month_num,
      month_overpay_vect = month_overpay_vect[[g$overpay_id]],
      reduce_term = g$reduce_term
    )

    # ---- enforce monthly outgoing cap (accurate) ----
    if (is.finite(max_monthly_outgoing)) {
      mp <- ms$monthly_payment_tot
      if (is.numeric(mp) && length(mp) == month_num + 1) {
        monthly_outgoing <- diff(mp[1:(month_num + 1)])
      } else if (is.numeric(mp) && length(mp) == month_num) {
        monthly_outgoing <- mp
      } else {
        stop("Cannot apply max_monthly_outgoing: `ms$monthly_payment_tot` has unexpected format.", call. = FALSE)
      }

      if (any(monthly_outgoing > max_monthly_outgoing, na.rm = TRUE)) {
        removed_cap <- removed_cap + 1L
        rows[[i]] <- NULL
        next
      }
    }

    capital_end <- ms$capital[month_num + 1]
    savings_end <- savings_after
    total_value_end_raw <- flat_value + savings_end - capital_end
    total_value_increase_raw <- total_value_end_raw - total_value_start

    monthly_payment_period <- ms$monthly_payment_tot[month_num + 1]
    interest_paid_period   <- ms$interest_paid_tot[month_num + 1]
    term_reduction_accrued <- ms$term_reduction_accrued[month_num + 1]
    capital_paid_period <- monthly_payment_period - interest_paid_period

    cost_upfront <- g$down_payment + upfront_fee_paid
    cost_monthly <- monthly_payment_period
    cost_total <- cost_upfront + cost_monthly

    ratio_total_cash <- if (cost_total == 0) NA_real_ else total_value_increase_raw / cost_total
    ratio_mortgage_cash <- if (cost_monthly == 0) NA_real_ else total_value_increase_raw / cost_monthly

    rows[[i]] <- data.frame(
      case_name = g$case_name,
      product_name = g$product_name,

      flat_value = flat_value,
      savings_start = savings,

      rate = g$rate,
      fees = g$fees,
      fixed_term_years = g$fixed_term_years,
      add_fees_to_loan = g$add_fees_to_loan,
      upfront_fee_paid = upfront_fee_paid,

      down_payment = g$down_payment,
      overpay_sum = overpay_sum,
      total_overpayment = total_overpayment,

      savings_horizon_start = savings_after,
      capital_start = capital_start_horizon,
      total_value_start = total_value_start,

      term_months = g$term_months,
      reduce_term = g$reduce_term,
      month_num = month_num,
      overpay_id = g$overpay_id,

      monthly_payment = ms$monthly_payment[month_num + 1],
      capital_paid_period = capital_paid_period,
      interest_paid_period = interest_paid_period,
      monthly_payment_period = monthly_payment_period,

      cost_upfront = cost_upfront,
      cost_monthly = cost_monthly,
      cost_total = cost_total,
      term_reduction_accrued = term_reduction_accrued,

      ratio_total_cash = ratio_total_cash,
      ratio_mortgage_cash = ratio_mortgage_cash,

      savings_end = savings_end,
      capital_end = capital_end,
      total_value_end_raw = total_value_end_raw,
      total_value_increase_raw = total_value_increase_raw
    )
  }

  out <- dplyr::bind_rows(rows)

  if (removed_cap > 0L) {
    warning(sprintf("Removed %d case(s): exceeded max_monthly_outgoing.", removed_cap), call. = FALSE)
  }
  if (nrow(out) == 0) {
    stop("All grid cases were removed (e.g., due to max_monthly_outgoing).", call. = FALSE)
  }

  # ---- Optional: normalize by monthly affordability only ----
  if (isTRUE(normalize_cashflow)) {
    max_cost_monthly <- max(out$cost_monthly, na.rm = TRUE)
    out <- out %>%
      dplyr::mutate(
        extra_cash_period = max_cost_monthly - .data$cost_monthly,
        total_value_end_monthly_norm = .data$total_value_end_raw + .data$extra_cash_period,
        total_value_increase_monthly_norm = .data$total_value_increase_raw + .data$extra_cash_period,
        ratio_monthly_budget = .data$total_value_increase_monthly_norm / max_cost_monthly
      )
  }

  out
}


# mortgage_savings_fees_comparison_grid <- function(
#     rate,
#     fees,
#     add_fees_to_loan,
#     capital,
#     term_months,
#     savings,
#     flat_value,
#     down_payment,
#     month_num,
#     month_overpay_vect,          # list of numeric vectors length month_num
#     reduce_term = TRUE,
#     case_name_fun = NULL,        # optional function(one_row_df) -> string
#     normalize_cashflow = TRUE
# ) {
#   # ---- validate overpay list ----
#   if (!is.list(month_overpay_vect) || length(month_overpay_vect) < 1) {
#     stop("`month_overpay_vect` must be a non-empty list of numeric vectors.", call. = FALSE)
#   }
#   ok <- vapply(month_overpay_vect, function(v) is.numeric(v) && length(v) == month_num, logical(1))
#   if (!all(ok)) {
#     bad <- which(!ok)
#     stop(
#       sprintf(
#         "All elements of `month_overpay_vect` must be numeric vectors of length `month_num` (%d). Bad indices: %s",
#         month_num, paste(bad, collapse = ", ")
#       ),
#       call. = FALSE
#     )
#   }
#
#   # ---- build grid ----
#   grid <- expand.grid(
#     rate = rate,
#     fees = fees,
#     add_fees_to_loan = add_fees_to_loan,
#     capital = capital,
#     term_months = term_months,
#     down_payment = down_payment,
#     reduce_term = reduce_term,
#     overpay_id = seq_along(month_overpay_vect),
#     stringsAsFactors = FALSE
#   )
#
#   # ---- default case naming ----
#   varying_cols <- vapply(
#     grid,
#     function(x) length(unique(x)) > 1,
#     logical(1)
#   )
#
#   format_value <- function(name, value) {
#
#     GBP_escaped = "\u00A3"
#
#     fmt_gbp <- function(x) {
#       # display as 200 or 200.50 depending on whether there are pennies
#       x <- round(as.numeric(x), 2)
#       if (is.na(x)) return(paste0(GBP_escaped,"NA"))
#       if (abs(x - round(x)) < 1e-9) {
#         paste0(GBP_escaped, format(round(x), big.mark = ",", scientific = FALSE))
#       } else {
#         paste0(GBP_escaped, format(x, nsmall = 2, big.mark = ",", scientific = FALSE))
#       }
#     }
#
#     overpay_desc <- function(overpay_id) {
#       v <- month_overpay_vect[[overpay_id]]
#
#       # Safety (should already be validated earlier)
#       if (!is.numeric(v) || length(v) != month_num) return(paste0("overpay#", overpay_id))
#
#       # Treat tiny floating noise as 0
#       v <- round(v, 2)
#       v[abs(v) < 1e-9] <- 0
#
#       if (all(v == 0)) return("no overpayment")
#
#       # Run-length encoding of piecewise-constant patterns
#       r <- rle(v)
#       vals <- r$values
#       lens <- r$lengths
#       ends <- cumsum(lens)
#       starts <- ends - lens + 1
#
#       # If constant non-zero
#       if (length(vals) == 1 && vals[1] != 0) {
#         return(paste0(fmt_gbp(vals[1]), " every month"))
#       }
#
#       # If exactly one non-zero month
#       nz_months <- which(v != 0)
#       if (length(nz_months) == 1) {
#         m <- nz_months[1]
#         return(paste0(fmt_gbp(v[m]), " on month ", m))
#       }
#
#       # Build minimal description from non-zero segments only
#       parts <- character(0)
#
#       for (k in seq_along(vals)) {
#         if (vals[k] == 0) next
#
#         s <- starts[k]
#         e <- ends[k]
#         amount <- fmt_gbp(vals[k])
#
#         # Single month segment
#         if (s == e) {
#           parts <- c(parts, paste0(amount, " on month ", s))
#           next
#         }
#
#         # Segment runs to the end => "every month from month s"
#         if (e == month_num) {
#           if (s == 1) {
#             parts <- c(parts, paste0(amount, " every month"))
#           } else {
#             parts <- c(parts, paste0(amount, " every month from month ", s))
#           }
#           next
#         }
#
#         # General multi-month segment not reaching the end
#         parts <- c(parts, paste0(amount, " on months ", s, "-", e))
#       }
#
#       paste(parts, collapse = " then ")
#     }
#
#     switch(
#       name,
#       rate = paste0("rate=", round(100 * value, 3), "%"),
#       fees = paste0("fee=", value),
#       add_fees_to_loan = if (value) "fees=financed" else "fees=upfront",
#       capital = paste0("cap=", value),
#       term_months = paste0("term=", value),
#       down_payment = paste0("dp=", value),
#       reduce_term = paste0("reduce_term=", value),
#       overpay_id = overpay_desc(value),
#       paste0(name, "=", value)  # fallback
#     )
#   }
#
#
#   if (is.null(case_name_fun)) {
#
#     cols_to_use <- names(varying_cols)[varying_cols]
#
#     grid$case_name <- vapply(seq_len(nrow(grid)), function(i) {
#       parts <- vapply(
#         cols_to_use,
#         function(col) format_value(col, grid[[col]][i]),
#         character(1)
#       )
#       paste(parts, collapse = " | ")
#     }, character(1))
#
#   } else {
#     grid$case_name <- vapply(seq_len(nrow(grid)), function(i) {
#       case_name_fun(grid[i, , drop = FALSE])
#     }, character(1))
#   }
#
#   # ---- run ----
#   rows <- vector("list", nrow(grid))
#
#   for (i in seq_len(nrow(grid))) {
#     g <- grid[i, ]
#
#     overpay_sum <- sum(month_overpay_vect[[g$overpay_id]], na.rm = TRUE)
#     total_overpayment <- g$down_payment + overpay_sum
#     upfront_fee_paid <- if (isTRUE(g$add_fees_to_loan)) 0 else g$fees
#     savings_after <- savings - g$down_payment - upfront_fee_paid
#
#     capital_increase_fees = if (isTRUE(g$add_fees_to_loan)) g$fees else 0
#     capital_start_horizon <- g$capital - g$down_payment + capital_increase_fees
#
#
#     if (capital_start_horizon < 0) {
#       stop(sprintf("Case %d: down_payment exceeds capital.", i), call. = FALSE)
#     }
#
#     total_value_start <- flat_value + savings_after - capital_start_horizon
#
#     ms <- mortgage_state_allmonths_vectorised(
#       rate = g$rate,
#       capital = capital_start_horizon,
#       term_months = g$term_months,
#       month_num = month_num,
#       month_overpay_vect = month_overpay_vect[[g$overpay_id]],
#       reduce_term = g$reduce_term
#     )
#
#     term_reduction_accrued = ms$term_reduction_accrued[month_num +1]
#
#     capital_end <- ms$capital[month_num + 1]
#     savings_end <- savings_after
#     total_value_end <- flat_value + savings_end - capital_end
#
#     monthly_payment_tot <- ms$monthly_payment_tot[month_num + 1]
#     interest_paid_tot   <- ms$interest_paid_tot[month_num + 1]
#     capital_paid_tot    <- monthly_payment_tot - interest_paid_tot
#
#     total_value_increase <- total_value_end - total_value_start
#
#     rows[[i]] <- data.frame(
#       case_name = g$case_name,
#
#       flat_value = flat_value,
#       savings_start = savings,
#
#       rate = g$rate,
#       fees = g$fees,
#       add_fees_to_loan = g$add_fees_to_loan,
#       upfront_fee_paid = upfront_fee_paid,
#       down_payment = g$down_payment,
#       total_monthly_overpayment = overpay_sum,
#       total_overpayment = total_overpayment,
#
#       savings_horizon_start = savings_after,
#       capital_start = capital_start_horizon,
#       capital_increase_fees = capital_increase_fees,
#       total_value_start = total_value_start,
#
#       term_months = g$term_months,
#       reduce_term = g$reduce_term,
#       month_num = month_num,
#       overpay_id = g$overpay_id,
#
#       monthly_payment = ms$monthly_payment[month_num + 1],
#       capital_paid_tot = capital_paid_tot,
#       interest_paid_tot = interest_paid_tot,
#       monthly_payment_tot = monthly_payment_tot,
#
#       savings_end = savings_end,
#       capital_end = capital_end,
#       term_reduction_accrued =term_reduction_accrued,
#       total_value_end = total_value_end,
#       total_value_increase = total_value_increase
#     )
#   }
#
#   out <- dplyr::bind_rows(rows)
#
#   if (isTRUE(normalize_cashflow)) {
#     out <- out %>%
#       dplyr::mutate(
#         outflow = .data$monthly_payment_tot + .data$capital_increase_fees+ .data$upfront_fee_paid,#+ .data$down_payment
#         extra_cash = max(.data$outflow) - .data$outflow,
#         total_value_end = .data$total_value_end + .data$extra_cash,
#         total_value_increase = .data$total_value_increase + .data$extra_cash,
#         value_increase_cost_ratio = .data$total_value_increase / .data$outflow
#       )
#   }
#
#   out
# }

#' @title mortgage_savings_fees_comparison
#'
#' @description gives summary of mortgage costs considering parameters, as a dataframe
#'
#' @param rate vector double, the mortgage rates (absolute value e.g. 1.5\% means rate = 0.015)
#' @param fees vector double, the mortgage fees
#' @param add_fees_to_loan vector boolean, whether to add the fees to the loan (TRUE) or pay it off as a single down payment (FALSE)
#' @param capital vector double, the mortgage remaining capital
#' @param term_months vector integer, number of months remaining to pay the mortgage back without overpayments
#' @param savings integer, amount in savings prior to duration
#' @param flat_value integer, value of the flat
#' @param down_payment vector integer, amount to take from savings and overpay ahead of loan
#' @param month_num integer, duration of payment period considered, in months
#' @param month_overpay_vect list vector double, amount overpaid each month, defaults to rep(0,month_num)
#' @param reduce_term vector boolean, defaults to TRUE do overpayments reduce terms
#' @param case_name vector string, defaults to "default" where the case_names will be "case_i"
#'
#' @return  dataframe with each observation being case and
#' variable including case_name, rate, capital_start, reduce_term, monthly_payment_tot, interest_paid_tot and capital_end
#' @export
#'
mortgage_savings_fees_comparison <- function(rate,
                                             fees,
                                             add_fees_to_loan,
                                             capital,
                                             term_months,
                                             savings,
                                             flat_value,
                                             down_payment,
                                             month_num,
                                             month_overpay_vect,
                                             reduce_term = TRUE,
                                             case_name = "default") {

  # ---- helpers ----
  recycle_to <- function(x, n, name) {
    if (length(x) == 0) stop(sprintf("`%s` must not be length 0.", name), call. = FALSE)
    if (length(x) == 1) return(rep(x, n))
    if (length(x) < n) stop(sprintf("`%s` has length %d but needs %d.", name, length(x), n), call. = FALSE)
    x[seq_len(n)]
  }

  # Determine nb_cases from ALL case-wise inputs
  case_lengths <- c(
    length(rate), length(fees), length(add_fees_to_loan), length(capital),
    length(term_months), length(down_payment), length(reduce_term),
    length(month_overpay_vect)
  )
  nb_cases <- min(case_lengths)

  if (nb_cases < 1) stop("No cases to run (nb_cases < 1).", call. = FALSE)

  # case_name handling
  if (length(case_name) == 1 && identical(case_name, "default")) {
    case_name <- paste0("case", seq_len(nb_cases))
  } else {
    case_name <- recycle_to(case_name, nb_cases, "case_name")
  }

  # Recycle case-wise vectors
  rate            <- recycle_to(rate, nb_cases, "rate")
  fees            <- recycle_to(fees, nb_cases, "fees")
  add_fees_to_loan<- recycle_to(add_fees_to_loan, nb_cases, "add_fees_to_loan")
  capital         <- recycle_to(capital, nb_cases, "capital")
  term_months     <- recycle_to(term_months, nb_cases, "term_months")
  down_payment    <- recycle_to(down_payment, nb_cases, "down_payment")
  reduce_term     <- recycle_to(reduce_term, nb_cases, "reduce_term")

  if (!is.list(month_overpay_vect)) {
    stop("`month_overpay_vect` must be a list of numeric vectors (one per case).", call. = FALSE)
  }

  # Validate and normalize month_overpay vectors to length month_num
  month_overpay_vect <- month_overpay_vect[seq_len(nb_cases)]
  month_overpay_vect <- lapply(seq_len(nb_cases), function(i) {
    v <- month_overpay_vect[[i]]
    if (length(v) == 1) v <- rep(v, month_num)
    if (length(v) < month_num) {
      stop(sprintf("month_overpay_vect[[%d]] has length %d but needs %d.", i, length(v), month_num), call. = FALSE)
    }
    as.numeric(v[seq_len(month_num)])
  })

  # ---- run cases ----
  rows <- vector("list", nb_cases)

  for (i in seq_len(nb_cases)) {

    # Apply choices at "start of horizon"
    upfront_fee_paid <- if (isTRUE(add_fees_to_loan[i])) 0 else fees[i]

    savings_start <- savings
    savings_after <- savings_start - down_payment[i] - upfront_fee_paid

    # Capital at start of simulation horizon (after down_payment, and fee financed if chosen)
    capital_start_horizon <- capital[i] - down_payment[i] + if (isTRUE(add_fees_to_loan[i])) fees[i] else 0

    if (capital_start_horizon < 0) {
      stop(sprintf("Case %d: down_payment (%.2f) exceeds capital (%.2f).", i, down_payment[i], capital[i]), call. = FALSE)
    }
    if (savings_after < 0) {
      warning(sprintf("Case %d: savings become negative after down_payment/fees (%.2f).", i, savings_after), call. = FALSE)
    }

    # Net worth snapshot at start of horizon (consistent with simulation start)
    total_value_start <- flat_value + savings_after - capital_start_horizon

    ms <- mortgage_state_allmonths_vectorised(
      rate = rate[i],
      capital = capital_start_horizon,
      term_months = term_months[i],
      month_num = month_num,
      month_overpay_vect = month_overpay_vect[[i]],
      reduce_term = reduce_term[i]
    )

    # End of horizon
    capital_end <- ms$capital[month_num + 1]
    savings_end <- savings_after
    total_value_end <- flat_value + savings_end - capital_end

    monthly_payment_tot <- ms$monthly_payment_tot[month_num + 1]
    interest_paid_tot   <- ms$interest_paid_tot[month_num + 1]
    capital_paid_tot    <- monthly_payment_tot - interest_paid_tot

    total_value_increase <- total_value_end - total_value_start
    ratio <- if (isTRUE(all.equal(monthly_payment_tot, 0))) NA_real_ else total_value_increase / monthly_payment_tot

    rows[[i]] <- data.frame(
      case_name = case_name[i],
      flat_value = flat_value,
      savings_start = savings_start,

      rate = rate[i],
      fees = fees[i],
      add_fees_to_loan = add_fees_to_loan[i],
      down_payment = down_payment[i],

      # "start of horizon" (after down_payment/fees)
      savings_horizon_start = savings_after,
      capital_start = capital_start_horizon,
      total_value_start = total_value_start,

      term_months = term_months[i],
      reduce_term = reduce_term[i],
      month_num = month_num,

      monthly_payment = ms$monthly_payment[month_num + 1],
      capital_paid_tot = capital_paid_tot,
      interest_paid_tot = interest_paid_tot,
      monthly_payment_tot = monthly_payment_tot,

      savings_end = savings_end,
      capital_end = capital_end,
      total_value_end = total_value_end,
      total_value_increase = total_value_increase,
      value_increase_cost_ratio = ratio
    )
  }

  output <- dplyr::bind_rows(rows)

  # ---- optional: normalize "extra cash" if you want all cases to have same outflow ----
  # (keeps your original intent, but made NA-safe)
  max_outflow <- max(output$monthly_payment_tot, na.rm = TRUE)
  output <- output %>%
    dplyr::mutate(
      extra_cash = max_outflow - .data$monthly_payment_tot,
      total_value_end = .data$total_value_end + .data$extra_cash,
      total_value_increase = .data$total_value_increase + .data$extra_cash,
      value_increase_cost_ratio = dplyr::if_else(
        .data$monthly_payment_tot == 0,
        NA_real_,
        .data$total_value_increase / .data$monthly_payment_tot
      )
    )

  output
}
# mortgage_savings_fees_comparison <- function(rate,
#                                              fees,
#                                              add_fees_to_loan,
#                                              capital,
#                                              term_months,
#                                              savings,
#                                              flat_value,
#                                              down_payment,
#                                              month_num,
#                                              month_overpay_vect,
#                                              reduce_term = TRUE,
#                                              case_name = "default") {
#
#   #TODO this does not consider the possibility that overpayments are used to reduce payments rather than reduce term
#   #this should be included as a parameter as it has been in other functions above
#
#   nb_cases = min(length(rate),
#                  length(capital),
#                  length(term_months),
#                  length(month_overpay_vect),
#                  length(reduce_term)
#   )
#
#   print(nb_cases)
#
#   if (length(case_name) == 1) {
#     if (case_name == "default") {
#       case_name = paste0("case", 1:nb_cases)
#     }
#   }
#
#   output <- data.frame(case_name = "",
#                        flat_value = 0,
#                        savings_start = 0,
#                        rate =0,
#                        fees = 0,
#                        add_fees_to_loan = FALSE,
#                        down_payment = 0,
#                        capital_start = 0,
#                        total_value_start = 0,
#                        term_months = 0,
#                        reduce_term=FALSE,
#                        month_num = 0,
#                        monthly_payment=0,
#                        capital_paid_tot=0,
#                        interest_paid_tot=0,
#                        monthly_payment_tot=0,
#                        savings_end = 0,
#                        capital_end=0,
#                        total_value_end = 0,
#                        total_value_increase = 0,
#                        value_increase_cost_ratio = 0)
#
#   for (case in 1:nb_cases) {
#     print(case)
#     capital_act = capital[case]-down_payment[case]
#     if (add_fees_to_loan[case]) {
#       capital_act = capital_act + fees[case]
#     }
#     case_msam =  mortgage_state_allmonths_vectorised(rate[case],
#                                                      capital_act,
#                                                      term_months[case],
#                                                      month_num,
#                                                      month_overpay_vect[[case]],
#                                                      reduce_term[case]
#     )
#
#     savings_start = savings
#     savings_end = savings - down_payment[case]
#     if (!add_fees_to_loan[case]) {
#       savings_end = savings_end - fees[case]
#     }
#
#     fees_downpayment = fees[case]
#     if (add_fees_to_loan[case]) {
#       fees_downpayment = 0
#     }
#
#     capital_start = capital[case]
#     if (add_fees_to_loan[case]) {
#       capital_start = capital_start + fees[case]
#     }
#     capital_end=case_msam$capital[month_num+1]
#
#
#     total_value_start = flat_value + savings_start -capital_start-fees_downpayment
#     monthly_payment_tot=case_msam$monthly_payment_tot[month_num+1]
#     total_value_end = flat_value + savings_end -capital_end
#
#     total_value_increase = total_value_end - total_value_start
#
#     additional_row = data.frame(case_name = case_name[case],
#                                 flat_value = flat_value,
#                                 savings_start = savings_start,
#
#                                 rate =rate[case],
#                                 fees = fees[case],
#                                 add_fees_to_loan = add_fees_to_loan[case],
#
#                                 down_payment = down_payment[case],
#
#                                 capital_start = capital_start,
#                                 total_value_start = total_value_start,
#                                 term_months = term_months[case],
#                                 reduce_term=reduce_term[case],
#                                 month_num = month_num,
#                                 monthly_payment = case_msam$monthly_payment[month_num+1],
#
#                                 interest_paid_tot=case_msam$interest_paid_tot[month_num+1],
#                                 capital_paid_tot=case_msam$monthly_payment_tot[month_num+1] - case_msam$interest_paid_tot[month_num+1],
#
#                                 monthly_payment_tot=monthly_payment_tot,
#                                 savings_end = savings_end,
#                                 capital_end=capital_end,
#                                 total_value_end = total_value_end,
#                                 total_value_increase = total_value_increase,
#                                 value_increase_cost_ratio = total_value_increase / monthly_payment_tot)
#
#
#     output <- dplyr::bind_rows(output,additional_row)
#
#
#   }
#   output <- dplyr::slice(output, -1)
#
#   output <- output %>%
#     mutate(extra_cash = max(.data$monthly_payment_tot) - .data$monthly_payment_tot) %>%
#     mutate(total_value_end = .data$total_value_end + .data$extra_cash) %>%
#     mutate(total_value_increase = .data$total_value_increase + .data$extra_cash) %>%
#     mutate(value_increase_cost_ratio = .data$total_value_increase / .data$monthly_payment_tot)
#
#
#   return(output)
# }


#' @title mortgage_savings_comparison
#'
#' @description gives summary of mortgage costs considering parameters, as a dataframe
#'
#' @param rate vector double, the mortgage rates (absolute value e.g. 1.5\% means rate = 0.015)
#' @param capital vector double, the mortgage remaining capital
#' @param term_months vector integer, number of months remaining to pay the mortgage back without overpayments
#' @param savings integer, amount in savings prior to duration
#' @param flat_value integer, value of the flat
#' @param down_payment vector integer, amount to take from savings and overpay ahead of loan
#' @param month_num integer, duration of payment period considered, in months
#' @param month_overpay_vect list vector double, amount overpaid each month, defaults to rep(0,month_num)
#' @param reduce_term vector boolean, defaults to TRUE do overpayments reduce terms
#' @param case_name vector string, defaults to "default" where the case_names will be "case_i"
#'
#' @return  dataframe with each observation being case and
#' variable including case_name, rate, capital_start, reduce_term, monthly_payment_tot, interest_paid_tot and capital_end
#' @export
#'
mortgage_savings_comparison <- function(rate,
                                        capital,
                                        term_months,
                                        savings,
                                        flat_value,
                                        down_payment,
                                        month_num,
                                        month_overpay_vect,
                                        reduce_term = TRUE,
                                        case_name = "default") {

  #TODO this does not consider the possibility that overpayments are used to reduce payments rather than reduce term
  #this should be included as a parameter as it has been in other functions above

  nb_cases = min(length(rate),
                 length(capital),
                 length(term_months),
                 length(month_overpay_vect),
                 length(reduce_term)
  )

  print(nb_cases)

  if (length(case_name) == 1) {
    if (case_name == "default") {
      case_name = paste0("case", 1:nb_cases)
    }
  }

  output <- data.frame(case_name = "",
                       flat_value = 0,
                       savings_start = 0,
                       rate =0,
                       capital_start = 0,
                       total_value_start = 0,
                       term_months = 0,
                       reduce_term=FALSE,
                       month_num = 0,
                       monthly_payment=0,
                       capital_paid_tot=0,
                       interest_paid_tot=0,
                       monthly_payment_tot=0,
                       savings_end = 0,
                       capital_end=0,
                       total_value_end = 0,
                       total_value_increase = 0,
                       value_increase_cost_ratio = 0)

  for (case in 1:nb_cases) {
    print(case)
    case_msam =  mortgage_state_allmonths_vectorised(rate[case],
                                                     capital[case]-down_payment[case],
                                                     term_months[case],
                                                     month_num,
                                                     month_overpay_vect[[case]],
                                                     reduce_term[case]
    )

    savings_start = savings
    savings_end = savings - down_payment[case]
    capital_start = capital[case]
    total_value_start = flat_value + savings_start -capital_start
    capital_end=case_msam$capital[month_num+1]
    total_value_end = flat_value + savings_end -capital_end
    total_value_increase = total_value_end - total_value_start
    monthly_payment_tot=case_msam$monthly_payment_tot[month_num+1]
    additional_row = data.frame(case_name = case_name[case],
                                flat_value = flat_value,
                                savings_start = savings_start,

                                rate =rate[case],
                                capital_start = capital_start,
                                total_value_start = total_value_start,
                                term_months = term_months[case],
                                reduce_term=reduce_term[case],
                                month_num = month_num,
                                monthly_payment = case_msam$monthly_payment[month_num+1],

                                interest_paid_tot=case_msam$interest_paid_tot[month_num+1],
                                capital_paid_tot=case_msam$monthly_payment_tot[month_num+1] - case_msam$interest_paid_tot[month_num+1],

                                monthly_payment_tot=monthly_payment_tot,
                                savings_end = savings_end,
                                capital_end=capital_end,
                                total_value_end = total_value_end,
                                total_value_increase = total_value_increase,
                                value_increase_cost_ratio = total_value_increase / monthly_payment_tot)


    output <- dplyr::bind_rows(output,additional_row)


  }
  output <- dplyr::slice(output, -1)
  return(output)
}


#' @title mortgage_comparison
#'
#' @description gives summary of mortgage costs considering parameters, as a dataframe
#'
#' @param rate vector double, the mortgage rates (absolute value e.g. 1.5\% means rate = 0.015)
#' @param capital vector double, the mortgage remaining capital
#' @param term_months vector integer, number of months remaining to pay the mortgage back without overpayments
#' @param month_num integer, duration of payment period considered, in months
#' @param month_overpay_vect list vector double, amount overpaid each month, defaults to rep(0,month_num)
#' @param reduce_term vector boolean, defaults to TRUE do overpayments reduce terms
#' @param case_name vector string, defaults to "default" where the case_names will be "case_i"
#'
#' @return  dataframe with each observation being case and
#' variable including case_name, rate, capital_start, reduce_term, monthly_payment_tot, interest_paid_tot and capital_end
#' @export
#'
mortgage_comparison <- function(rate,
                                capital,
                                term_months,
                                month_num,
                                month_overpay_vect,
                                reduce_term = TRUE,
                                case_name = "default") {

  #TODO this does not consider the possibility that overpayments are used to reduce payments rather than reduce term
  #this should be included as a parameter as it has been in other functions above

  nb_cases = min(length(rate),
                 length(capital),
                 length(term_months),
                 length(month_overpay_vect),
                 length(reduce_term)
                 )

  print(nb_cases)

  if (length(case_name) == 1) {
    if (case_name == "default") {
      case_name = paste0("case", 1:nb_cases)
    }
  }

  output <- data.frame(case_name = "",
                       rate =0,
                       capital_start = 0,
                       term_months = 0,
                       reduce_term=FALSE,
                       month_num = 0,
                       monthly_payment=0,
                       monthly_payment_tot=0,
                       interest_paid_tot=0,
                       capital_paid_tot=0,
                       capital_end=0)

  for (case in 1:nb_cases) {
    print(case)
    case_msam =  mortgage_state_allmonths_vectorised(rate[case],
                                                     capital[case],
                                                     term_months[case],
                                                     month_num,
                                                     month_overpay_vect[[case]],
                                                     reduce_term[case]
                                                    )

    additional_row = data.frame(case_name = case_name[case],
                       rate =rate[case],
                       capital_start = capital[case],
                       term_months = term_months[case],
                       reduce_term=reduce_term[case],
                       month_num = month_num,
                       monthly_payment = case_msam$monthly_payment[month_num+1],
                       monthly_payment_tot=case_msam$monthly_payment_tot[month_num+1],
                       interest_paid_tot=case_msam$interest_paid_tot[month_num+1],
                       capital_paid_tot=case_msam$monthly_payment_tot[month_num+1] - case_msam$interest_paid_tot[month_num+1],
                       capital_end=case_msam$capital[month_num+1])


    output <- dplyr::bind_rows(output,additional_row)


  }
  output <- dplyr::slice(output, -1)
  return(output)
}

#' @title mortgage_state_allmonths_vectorised
#'
#' @description shows state of mortgage as a dataframe with each observation being one month and
#' variable including month, monthly_payment, overpayment, monthly_payment_tot, interest_paid,
#' interest_paid_tot and capital
#'
#' @param rate double, the mortgage current rate (absolute value e.g. 1.5\% means rate = 0.015)
#' @param capital double, the mortgage remaining capital
#' @param term_months integer, number of months remaining to pay the mortgage back without overpayments
#' @param month_num integer, duration of payment period considered, in months
#' @param month_overpay_vect vector double, amount overpaid each month, defaults to rep(0,month_num)
#' @param reduce_term boolean, defaults to TRUE do overpayments reduce terms
#'
#' @return  dataframe with each observation being one month and
#' variable including month, monthly_payment, overpayment, monthly_payment_tot, interest_paid,
#' interest_paid_tot and capital
#' @export
#'
mortgage_state_allmonths_vectorised <- function(rate,
                                                capital,
                                                term_months,
                                                month_num,
                                                month_overpay_vect = rep(0,month_num),
                                                reduce_term = TRUE) {

  #TODO this does not consider the possibility that overpayments are used to reduce payments rather than reduce term
  #this should be included as a parameter as it has been in other functions above

  output <- data.frame(month =0,
                       monthly_payment_base = 0,
                       overpayment = 0,
                       monthly_payment = 0,
                       monthly_payment_tot = 0,
                       interest_paid = 0,
                       interest_paid_tot = 0,
                       capital = capital,
                       remaining_term_months = term_months,
                       implied_term_months = term_months)
  m_p <- monthly_payment(rate, capital, term_months)
  m_i_p_tot <- 0
  m_p_actual_tot <- 0
  for (month in 1:month_num) {
    if (!reduce_term) {
      m_p <- monthly_payment(rate, capital, term_months)
    }
    month_overpay_act <- month_overpay_vect[month]
    m_p_actual <- m_p + month_overpay_act
    m_p_actual_tot <- m_p_actual_tot + m_p_actual
    m_i_p <- monthly_interest_paid(rate,capital)
    m_i_p_tot <- m_i_p_tot + m_i_p
    c_p <- m_p_actual - m_i_p
    if (capital-c_p < 0) {
      c_p <- capital
      m_p_actual <- c_p + m_i_p
    }
    capital <- capital - c_p
    itm =  ceiling(implied_term_months(rate,capital,m_p))
    output <- dplyr::bind_rows(output,c(month = month,
                                     monthly_payment_base = m_p,
                                     overpayment = month_overpay_act,
                                     monthly_payment = m_p_actual,
                                     monthly_payment_tot = m_p_actual_tot,
                                     interest_paid = m_i_p,
                                     interest_paid_tot = m_i_p_tot,
                                     capital = capital,
                                     remaining_term_months = term_months - month,
                                     implied_term_months = itm
                                     ))

    output <- output %>% mutate(term_reduction_accrued = term_months - .data$month - .data$implied_term_months)
    if (capital == 0) {
      warning("capital reduced to zero")
      break
    }
  }
  return(output)
}

#' @title mortgage_state_allmonths
#'
#' @description shows state of mortgage as a dataframe with each observation being one month and
#' variable including month, monthly_payment, overpayment, monthly_payment_tot, interest_paid,
#' interest_paid_tot and capital
#'
#' @param rate double, the mortgage current rate (absolute value e.g. 1.5\% means rate = 0.015)
#' @param capital double, the mortgage remaining capital
#' @param term_months integer, number of months remaining to pay the mortgage back without overpayments
#' @param month_num integer, duration of payment period considered, in months
#' @param month_overpay double, amount overpaid each month, defaults to 0
#' @param month_overpay_nums vector, with index having non NA values will result in overpayment being applied for that month.
#' hence 1:month_num (default value) will mean overpayment for the whole payment period considered
#' @param reduce_term boolean, defaults to TRUE do overpayments reduce terms
#'
#' @return  dataframe with each observation being one month and
#' variable including month, monthly_payment, overpayment, monthly_payment_tot, interest_paid,
#' interest_paid_tot and capital
#' @export
#'
mortgage_state_allmonths <- function(rate,
                                     capital,
                                     term_months,
                                     month_num,
                                     month_overpay = 0,
                                     month_overpay_nums = 1:month_num,
                                     reduce_term = TRUE) {

  #TODO a vectorised month_overpay would make more sense and less parameters as month_overpay_nums could go

  #TODO this does not consider the possibility that overpayments are used to reduce payments rather than reduce term
  #this should be included as a parameter as it has been in other functions above

  output <- data.frame(month =0,
                       monthly_payment = 0,
                       overpayment = 0, monthly_payment_tot = 0, interest_paid = 0, interest_paid_tot = 0, capital = capital)
  m_p <- monthly_payment(rate, capital, term_months)
  m_i_p_tot <- 0
  for (month in 1:month_num) {
    if (!reduce_term) {
      m_p <- monthly_payment(rate, capital, term_months)
    }
    month_overpay_act <- month_overpay * dplyr::if_else(is.na(month_overpay_nums[month]),0,1)
    m_p_actual <- m_p + month_overpay_act
    m_i_p <- monthly_interest_paid(rate,capital)
    m_i_p_tot <- m_i_p_tot + m_i_p
    c_p <- m_p_actual - m_i_p
    if (capital-c_p < 0) {
      c_p <- capital
      m_p_actual <- c_p + m_i_p
    }
    capital <- capital - c_p
    output <- dplyr::bind_rows(output,c(month = month,
                                        monthly_payment = m_p,
                                        overpayment = month_overpay_act,
                                        monthly_payment_tot = m_p_actual,
                                        interest_paid = m_i_p,
                                        interest_paid_tot = m_i_p_tot,
                                        capital = capital))
    if (capital == 0) {
      warning("capital reduced to zero")
      break
    }
  }
  return(output)
}


#' @title actual_term_months
#'
#' @description actual duration of mortgage which will either be equal to term_months or less
#' in the case that there is overpaymnets
#'
#' @param rate double, the mortgage current rate (absolute value e.g. 1.5\% means rate = 0.015)
#' @param capital double, the mortgage remaining capital
#' @param term_months integer, number of months remaining to pay the mortgage back
#' @param month_overpay integer, amount overpaid each month, defaults to 0
#' @param month_overpay_nums vector, with index having non NA values will result in overpayment being applied for that month.
#' hence 1:month_num (default value) will mean overpayment for the whole payment period considered
#'
#'
#' @return integer actual duration of mortgage in months
#' @export
#'
actual_term_months <- function(rate,
                               capital,
                               term_months,
                               month_overpay = 0,
                               month_overpay_nums=1:term_months) {
  mpsa <- mortgage_state_allmonths(rate,capital,term_months,month_num = term_months*12,month_overpay,month_overpay_nums)
  term_months_actual <- max(mpsa$month)
  return(term_months_actual)
}

#' @title total_term_reduction_months
#'
#' @description number of months removed from the mortgage duration compared to the case
#' where there is no overpayments
#'
#' @param rate double, the mortgage current rate (absolute value e.g. 1.5\% means rate = 0.015)
#' @param capital double, the mortgage remaining capital
#' @param term_months integer, number of months remaining to pay the mortgage back
#' @param month_overpay integer, amount overpaid each month, defaults to 0
#' @param month_overpay_nums vector, with index having non NA values will result in overpayment being applied for that month.
#' hence 1:month_num (default value) will mean overpayment for the whole payment period considered
#'
#'
#' @return integer number of months removed from the mortgage duration
#' @export
#'
total_term_reduction_months <- function(rate,
                                        capital,
                                        term_months,
                                        month_overpay = 0,
                                        month_overpay_nums=1:term_months) {

  output <- 0

  if (month_overpay != 0) {
    mpsa <- mortgage_state_allmonths(rate,capital,term_months,month_num = term_months*12,month_overpay,month_overpay_nums)
    term_months_actual <- max(mpsa$month)
    output <- term_months - term_months_actual
  }

  return(output)
}


#' @title price_fixedterm
#'
#' @description calculates the price of a fixed term of a mortgage
#'
#' @param rate double, the mortgage current rate (absolute value e.g. 1.5\% means rate = 0.015)
#' @param capital double, the mortgage remaining capital
#' @param term_months integer, number of months remaining to pay the mortgage back
#' @param duration_years integer, duration of fixed term in years
#' @param fee double, the mortgage upfront fee
#' @param rest integer, defaults to 12 for monthly payments
#'
#' @return double price of fixed term
#' @export
#'
price_fixedterm <- function(rate, capital, term_months,duration_years, fee = 0,rest = 12) {
  output <- rest* duration_years * monthly_payment(rate, capital, term_months,rest) + fee
  return(output)
}
