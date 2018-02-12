context("Authentication")

test_that("Authentication handles missing values", {
  expect_error(bwr_auth(un = c("mickey", "goofy"), pw = "ourpassword", refresh = TRUE), "You must provide a username as a string of length one")
  expect_error(bwr_auth(un = "mickey", pw = c("onepassword", "twopassword"), refresh = TRUE), "You must provide a password as a string of length one")
})

