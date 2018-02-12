
# mentions_get ------------------------------------------------------------

context("Mentions")

test_that("mentions_get handles missing token values", {
  expect_error(bwr_mentions_get(token = 234234), "Token object does not appear")
  expect_error(bwr_mentions_get(token = c(1:10)), "Token object does not appear")
})

test_that("mentions_get handles missing / invalid project values", {
  expect_error(bwr_mentions_get(token = "test_token", query_id = 23423, project_id = 1:1), "project.*must")
  expect_error(bwr_mentions_get(token = "test_token", query_id = 23423, project_id = Sys.Date()), "project.*must")
  expect_error(bwr_mentions_get(token = "test_token", query_id = 23423, project_id = NULL), "project.*must")
})


test_that("mentions_get handles missing / invalid query values", {
  expect_error(bwr_mentions_get(token = "test_token", query_id = 1:10, project_id = "test_project"), "query.*must")
  expect_error(bwr_mentions_get(token = "test_token", query_id = Sys.Date(), project_id = "test_project"), "query.*must")
  expect_error(bwr_mentions_get(token = "test_token", query_id = NULL, project_id = "test_project"), "query.*must")
})


test_that("mentions_get handles missing / invalid date_range values", {
  expect_error(bwr_mentions_get(token = "test_token",
                                query_id = 1124124,
                                project_id = "test_project",
                                date_range = 1:10), "date.*must")
  expect_error(bwr_mentions_get(token = "test_token",
                                query_id = 1124124,
                                project_id = "test_project",
                                date_range = NULL), "date.*must")
  expect_error(bwr_mentions_get(token = "test_token",
                                query_id = 1124124,
                                project_id = "test_project",
                                date_range = c("2017-01-01")), "date.*must")
})


# mentions_total ----------------------------------------------------------

test_that("mentions_total handles missing token values", {
  expect_error(bwr_mentions_total(token = 234234), "Token object does not appear")
  expect_error(bwr_mentions_total(token = c(1:10)), "Token object does not appear")
})

test_that("mentions_total handles missing / invalid project values", {
  expect_error(bwr_mentions_total(token = "test_token", query_id = 23423, project_id = 1:1), "project.*must")
  expect_error(bwr_mentions_total(token = "test_token", query_id = 23423, project_id = Sys.Date()), "project.*must")
  expect_error(bwr_mentions_total(token = "test_token", query_id = 23423, project_id = NULL), "project.*must")
})


test_that("mentions_total handles missing / invalid query values", {
  expect_error(bwr_mentions_total(token = "test_token", query_id = 1:10, project_id = "test_project"), "query.*must")
  expect_error(bwr_mentions_total(token = "test_token", query_id = Sys.Date(), project_id = "test_project"), "query.*must")
  expect_error(bwr_mentions_total(token = "test_token", query_id = NULL, project_id = "test_project"), "must.*provide")
})

test_that("mentions_total handles missing / invalid querygrp values", {
  expect_error(bwr_mentions_total(token = "test_token", querygrp_id = 1:10, project_id = "test_project"), "query.*must")
  expect_error(bwr_mentions_total(token = "test_token", querygrp_id = Sys.Date(), project_id = "test_project"), "query.*must")
  expect_error(bwr_mentions_total(token = "test_token", querygrp_id = NULL, project_id = "test_project"), "must.*provide")
})

test_that("mentions_total handles missing / invalid date_range values", {
  expect_error(bwr_mentions_total(token = "test_token",
                                query_id = 1124124,
                                project_id = "test_project",
                                date_range = 1:10), "date.*must")
  expect_error(bwr_mentions_total(token = "test_token",
                                query_id = 1124124,
                                project_id = "test_project",
                                date_range = NULL), "date.*must")
  expect_error(bwr_mentions_total(token = "test_token",
                                query_id = 1124124,
                                project_id = "test_project",
                                date_range = c("2017-01-01")), "date.*must")
})



# mentions_topics ----------------------------------------------------------

test_that("mentions_topics handles missing token values", {
  expect_error(bwr_mentions_topics(token = 234234), "Token object does not appear")
  expect_error(bwr_mentions_topics(token = c(1:10)), "Token object does not appear")
})

test_that("mentions_topics handles missing / invalid project values", {
  expect_error(bwr_mentions_topics(token = "test_token", query_id = 23423, project_id = 1:1), "project.*must")
  expect_error(bwr_mentions_topics(token = "test_token", query_id = 23423, project_id = Sys.Date()), "project.*must")
  expect_error(bwr_mentions_topics(token = "test_token", query_id = 23423, project_id = NULL), "project.*must")
})


test_that("mentions_topics handles missing / invalid query values", {
  expect_error(bwr_mentions_topics(token = "test_token", query_id = 1:10, project_id = "test_project"), "query.*must")
  expect_error(bwr_mentions_topics(token = "test_token", query_id = Sys.Date(), project_id = "test_project"), "query.*must")
  expect_error(bwr_mentions_topics(token = "test_token", query_id = NULL, project_id = "test_project"), "must.*provide")
})

test_that("mentions_topics handles missing / invalid querygrp values", {
  expect_error(bwr_mentions_topics(token = "test_token", querygrp_id = 1:10, project_id = "test_project"), "query.*must")
  expect_error(bwr_mentions_topics(token = "test_token", querygrp_id = Sys.Date(), project_id = "test_project"), "query.*must")
  expect_error(bwr_mentions_topics(token = "test_token", querygrp_id = NULL, project_id = "test_project"), "must.*provide")
})

test_that("mentions_topics handles missing / invalid date_range values", {
  expect_error(bwr_mentions_topics(token = "test_token",
                                  query_id = 1124124,
                                  project_id = "test_project",
                                  date_range = 1:10), "date.*must")
  expect_error(bwr_mentions_topics(token = "test_token",
                                  query_id = 1124124,
                                  project_id = "test_project",
                                  date_range = NULL), "date.*must")
  expect_error(bwr_mentions_topics(token = "test_token",
                                  query_id = 1124124,
                                  project_id = "test_project",
                                  date_range = c("2017-01-01")), "date.*must")
})

