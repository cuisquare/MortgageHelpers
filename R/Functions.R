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


#' @title actual_term_month
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
