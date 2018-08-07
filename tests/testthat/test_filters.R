context("Filters")

test_that("bwr_filters_get handles token issues", {
  expect_error(bwr_cat_get(token = 234234), "Token object does not appear")
  expect_error(bwr_cat_get(token = c(1:10)), "Token object does not appear")
})

test_that("bwr_metrics_get handles token issues", {
  expect_error(bwr_cat_get(token = 234234), "Token object does not appear")
  expect_error(bwr_cat_get(token = c(1:10)), "Token object does not appear")
})
