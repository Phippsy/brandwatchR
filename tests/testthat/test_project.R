context("Projects")

test_that("Project requests handle missing values", {
  expect_error(bwr_projects_get(token = 234234), "Token object does not appear")
  expect_error(bwr_projects_get(token = c(1:10)), "Token object does not appear")
})
