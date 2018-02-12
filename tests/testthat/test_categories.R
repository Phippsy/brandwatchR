context("Categories")

test_that("cat_get handles missing values", {
  expect_error(bwr_cat_get(token = 234234), "Token object does not appear")
  expect_error(bwr_cat_get(token = c(1:10)), "Token object does not appear")
  expect_error(bwr_cat_get(token = "test_token"), "valid project")
})

test_that("cat_create handles missing values", {
  expect_error(bwr_cat_create(token = 234234), "Token object does not appear")
  expect_error(bwr_cat_create(token = c(1:10)), "Token object does not appear")
  expect_error(bwr_cat_create(token = "test_token"), "valid project")
  expect_error(bwr_cat_create(token = "test_token", project = "my_project", name = NULL), "valid name")
})
