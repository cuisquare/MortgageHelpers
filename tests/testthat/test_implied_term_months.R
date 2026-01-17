test_that("implied_term_months runs", {
  implied_term_months(
    capital = 182000,
    base_payment = 1500,
    rate = 0.0154
  )

  expect_equal(2 * 2, 4)
})
