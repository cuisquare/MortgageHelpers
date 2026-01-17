test_that("mortgage_comparison runs", {
  capital_start_tot = 142000
  new_term_months = 10*12
  new_month_num = 24
  new_rate1 = 0.0399
  new_rate2 = 0.0369
  #mortgages comparison
  mc_rates = mortgage_comparison(rate = c(new_rate1, new_rate2),
                                 capital = c(capital_start_tot,capital_start_tot),
                                 term_months = c(new_term_months, new_term_months),
                                 month_num = new_month_num,
                                 month_overpay_vect = list(rep(200, new_month_num),rep(200, new_month_num)),
                                 reduce_term = c(TRUE, TRUE),
                                 case_name = c("rate1", "rate2")
  )

  expect_equal(2 * 2, 4)
})
