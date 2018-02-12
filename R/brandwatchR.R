#' brandwatchR: A package for retrieving data from the Brandwatch API.
#'
#' The brandwatchR package provides families of wrapper functions for the Brandwatch API.
#'
#' @section Authentication:
#' \itemize{
#'   \item{\code{\link{bwr_auth}}} allows the user to authenticate and cache authentication information for future API calls.
#' }
#'
#' @section Projects:
#' \itemize{
#'   \item{\code{\link{bwr_projects_get}}} - get a dataframe of projects
#' }
#'
#' @section Queries / mentions:
#' \itemize{
#'   \item{\code{\link{bwr_query_get}}} - get a dataframe of queries for a project
#'   \item{\code{\link{bwr_query_check}}} - check the syntax of a query
#'   \item{\code{\link{bwr_query_create}}} - upload a new query
#'   \item{\code{\link{bwr_query_delete}}} - delete a specified query
#'   \item{\code{\link{bwr_mentions_get}}} - get the mentions for a specified query
#'   \item{\code{\link{bwr_mentions_total}}} - get the total number of mentions for a specified query or query group.
#'   \item{\code{\link{bwr_mentions_topics}}} - get the matching topics & topic metadata for a specified query or query group.
#'   \item{\code{\link{bwr_mentions_topsites}}} - get the top authors for a specified query or query group.
#'   \item{\code{\link{bwr_mentions_toptweeters}}} - get the top tweeters for a specified query or query group.
#' }
#'
#' @section Query filters:
#' \itemize{
#'   \item{\code{\link{bwr_filters_get}}} - Get a data frame of all parameters which can be used to filter your query
#'   \item{\code{\link{bwr_metrics_get}}} - Get a data frame of all metrics which have limited accepted values.
#' }
#;
#' @section Query Groups:
#' \itemize{
#'   \item{\code{\link{bwr_querygrp_get}}} - get the query groups for a specified project
#'   \item{\code{\link{bwr_querygrp_delete}}} - delete a specfied query group
#' }
#'
#' @section Tags:
#' \itemize{
#'   \item{\code{\link{bwr_tag_get}}} - get the tags for a specified project
#'   \item{\code{\link{bwr_tag_create}}} - create a new tag for a specified project
#'   \item{\code{\link{bwr_tag_delete}}} - delete a tag for a specified project
#' }
#'
#' @section Categories:
#' \itemize{
#'   \item{\code{\link{bwr_cat_get}}} - get the categories for a specified project
#'   \item{\code{\link{bwr_cat_create}}} - create a new category for a specified project
#' }
#'
#' @docType package
#' @name brandwatchR
NULL
