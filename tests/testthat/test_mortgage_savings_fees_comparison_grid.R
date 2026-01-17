test_that("mortgage_comparison runs", {
  capital_start_tot = 182960.11
  savings = 45000
  flat_value = 360000
  max_monthly_outgoing = 1800 # Inf
  extra_cash_monthly_for_overpay_target = 200

  monthly_overpayment1 = 0
  monthly_overpayment2 = 0

  new_rateA = 0.0364
  new_rateB = 0.0399

  down_payment1 = 42960.11
  down_payment2 = 0

  new_term_months = 10*12

  new_month_num =  12 # 12
  total_extra_cash_monthly_for_overpay_target = extra_cash_monthly_for_overpay_target * new_month_num

  products <- dplyr::bind_rows(
    mortgage_product("NATWEST_2yr_fix_fees",            rate = 0.0364, fees = 1025,fixed_term_years = 2),
    mortgage_product("NATWEST_2yr_fix_lowfees",           rate = 0.0398, fees = 30,fixed_term_years = 2),
    mortgage_product("BARCLAYS_2yr_fix_lowfees",           rate = 0.0400, fees = 35,fixed_term_years = 2),
    mortgage_product("SANTANDER_2yr_fix_fees",           rate = 0.0368, fees = 999,fixed_term_years = 2),
    mortgage_product("SANTANDER_2yr_fix_nofees",    rate = 0.0396, fees = 0,fixed_term_years = 2),
    mortgage_product("HSBC_2yr_fix_nofees",         rate = 0.0399, fees = 0,fixed_term_years = 2)
  )

  #gird version products
  mscf_grid_all_products <- mortgage_savings_fees_comparison_grid(
    rate = NA, #c(new_rateA, new_rateB),
    fees = NA, #c(1025, 0),
    add_fees_to_loan = c(TRUE, FALSE),
    fixed_term_years = 2,
    capital = capital_start_tot,
    term_months = new_term_months,
    savings = savings,
    flat_value = flat_value,
    down_payment = c(down_payment1),
    month_num = new_month_num,
    month_overpay_vect = list(rep(0, new_month_num),
                              rep(extra_cash_monthly_for_overpay_target, new_month_num)
    ),
    reduce_term = TRUE,
    max_monthly_outgoing = max_monthly_outgoing,
    products = products
  )

  expect_equal(2 * 2, 4)
})
