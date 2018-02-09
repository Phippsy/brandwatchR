
#' Get a list of mentions for the specified query and project
#'
#' @param project_id
#' The project id in which the specified query is contained. Obtain a list of project IDs using bwr_get_projects().
#' @param query_id
#' The id of the query you'd like to run. Obtain a list of queries for any specified project using bwr_get_queries().
#' @param date_range
#' A character vector containing 2 date values in YYYY-mm-dd format. The first value is the beginning of your desired date range and the second value is the end of the date range.
#' @param page
#' (Optional) The page number for which to return results. This is needed only when R needs to iterate through multiple pages of results from the Brandwatch API. It is recommended that you keep this value set as zero.
#' @param token
#' The authentication token, acquired using bwr_auth()
#' @param filters
#' (Optional) A list of key-value pairs to filter the mentions query by. Use the bwr_filters_get() function to find out all available filters.
#' @return
#' Returns a data frame containing all results
#' @export
#'
#' @examples
#' my_project <- bwr_get_projects()$id[1]
#' my_query <- bwr_get_queries(project_id = my_project)$id[1]
#' my_mentions <- bwr_mentions_get(
#' project_id = my_project,
#' query_id = my_query,
#' date_range = c("2018-01-01", "2018-02-01"))
#'
bwr_mentions_get <- function(token = Sys.getenv("BW_TOKEN"),
                             project_id = NULL,
                            query_id = NULL,
                            filters= NULL,
                            date_range = c(Sys.Date()-31, Sys.Date()-1),
                            page = NULL) {
  print(paste("page = ", page))
  url <- paste0("https://api.brandwatch.com/projects/",
                project_id,
                "/data/mentions")
  r <- httr::GET(url,
                 query = c(list(queryId = query_id,
                              startDate = date_range[1],
                              endDate = date_range[2],
                              pageSize = 5000,
                              page = page,
                              access_token = token),
                           filter))
  httr::stop_for_status(r)

  # Parse the results and return
  con <- httr::content(r, "text")
  json <- jsonlite::fromJSON(con)
  results <- json$results

  # Deal with paging - if results >= 5000, check next page
  if (dim(results)[1] >= 5000 ) {
    results <- data.table::rbindlist(
      list(
        results,
        bwr_get_mentions(project_id = project_id,
                         query_id = query_id,
                         date_range = date_range,
                         page = json$resultsPage+1)),
      fill = TRUE)
  }

  results
}

#' Get the total mentions for a specified Brandwatch query or query group
#'
#' @param project_id
#' The project id in which the specified query is contained. Obtain a list of project IDs using bwr_get_projects().
#' @param query_id
#' The query ID you'd like to see total mentions for. Note that you can only specify a query or a query group, not both.
#' @param date_range
#' A character vector containing 2 date values in YYYY-mm-dd format. The first value is the beginning of your desired date range and the second value is the end of the date range.
#' @param querygrp_id
#' The query group ID you'd like to see total mentions for. Note that you can only specify a query or a query group, not both.
#' @param token
#' The authentication token, acquired using bwr_auth()
#' @param filters
#' (Optional) A list of key-value pairs to filter the mentions query by. Use the bwr_filters_get() function to find out all available filters.
#' @return
#' Returns a numeric vector of length 1 with the results.
#' @export
#'
#' @examples
#' my_project <- bwr_projects_get()$id[1]
#' my_query <- bwr_queries_get(project_id = my_project)$id[1]
#' my_mentions <- bwr_mentions_total(
#' project_id = my_project,
#' query_id = my_query,
#' date_range = c("2018-01-01", "2018-02-01"))
#'
bwr_mentions_total <- function(project_id = NULL,
                             query_id = NULL,
                             querygrp_id = NULL,
                             filters= NULL,
                             date_range = c(Sys.Date()-31, Sys.Date()-1),
                             token = Sys.getenv("BW_TOKEN")) {
  url <- paste0("https://api.brandwatch.com/projects/",
                project_id,
                "/data/mentions/count")
  query <-  c(list(startDate = date_range[1],
                 endDate = date_range[2],
                 access_token = token,
                 queryId = query_id,
                 queryGroupId = querygrp_id),
              filter)

  r <- httr::GET(url,
                 query = query)
  httr::stop_for_status(r)

  # Parse the results and return
  con <- httr::content(r, "text")
  json <- jsonlite::fromJSON(con)
  json$mentionsCount
}


#' Get a dataframe of topic data for a specified Brandwatch query or query group
#'
#' @param project_id
#' The project id in which the specified query is contained. Obtain a list of project IDs using bwr_get_projects().
#' @param query_id
#' The query ID you'd like to see total mentions for. Note that you can only specify a query or a query group, not both.
#' @param date_range
#' A character vector containing 2 date values in YYYY-mm-dd format. The first value is the beginning of your desired date range and the second value is the end of the date range.
#' @param querygrp_id
#' The query group ID you'd like to see total mentions for. Note that you can only specify a query or a query group, not both.
#' @param token
#' The authentication token, acquired using bwr_auth()
#' @param filters
#' (Optional) A list of key-value pairs to filter the mentions query by. Use the bwr_filters_get() function to find out all available filters.
#' @param order_by
#' Metric by which you wish to order the topics returned. Can be either "volume" (the number of Mentions the topic appears in) or "burst" (the rate that the topic has emerged over time).
#' @param limit
#' The maximum number of topics to be returned.
#' @param exclude_categories
#' You have the option to exclude Categories from the topics returned (this is often done in the Brandwatch Analytics UI).
#' @param exclude_tags
#' The authentication token, acquired using bwr_auth()
#' @return
#' Returns a data frame (including some list-columns) of the JSON results, showing information about topics related to your query.
#' @export
#'
#' @examples
#' my_project <- bwr_projects_get()$id[1]
#' my_query <- bwr_query_get(project_id = my_project)$id[1]
#' my_mentions <- bwr_mentions_topics(
#' project_id = my_project,
#' query_id = my_query,
#' date_range = c("2018-01-01", "2018-02-01"))
bwr_mentions_topics <- function(project_id = NULL,
                                query_id = NULL,
                                querygrp_id = NULL,
                                date_range = c(Sys.Date()-31, Sys.Date()-1),
                                order_by = NULL,
                                limit = NULL,
                                exclude_categories = NULL,
                                exclude_tags = NULL,
                                filters = NULL,
                                token = Sys.getenv("BW_TOKEN")) {
  url <- paste0("https://api.brandwatch.com/projects/",
                project_id,
                "/data/volume/topics/queries")
  query <-  c(list(startDate = date_range[1],
                 endDate = date_range[2],
                 access_token = token,
                 queryId = query_id,
                 queryGroupId = querygrp_id,
                 orderBy = order_by,
                 limit = limit,
                 excludeCategories = exclude_categories,
                 excludeTags = exclude_tags),
              filters)

  r <- httr::GET(url,
                 query = query)
  httr::stop_for_status(r)

  # Parse the results and return
  con <- httr::content(r, "text")
  json <- jsonlite::fromJSON(con)
  json$topics
}




#' Get a dataframe of top site data for a specified Brandwatch query or query group
#'
#' @param project_id
#' The project id in which the specified query is contained. Obtain a list of project IDs using bwr_get_projects().
#' @param query_id
#' The query ID you'd like to see total mentions for. Note that you can only specify a query or a query group, not both.
#' @param date_range
#' A character vector containing 2 date values in YYYY-mm-dd format. The first value is the beginning of your desired date range and the second value is the end of the date range.
#' @param querygrp_id
#' The query group ID you'd like to see total mentions for. Note that you can only specify a query or a query group, not both.
#' @param token
#' The authentication token, acquired using bwr_auth()
#' @param filters
#' (Optional) A list of key-value pairs to filter the mentions query by. Use the bwr_filters_get() function to find out all available filters.
#'
#' @return
#' Returns a data frame (including some list-columns) of the JSON results, showing information about the top sites related to your query.
#' @export
#'
#' @examples
#' my_project <- bwr_projects_get()$id[1]
#' my_query <- bwr_query_get(project_id = my_project)$id[1]
#' my_mentions <- bwr_mentions_topsites(
#' project_id = my_project,
#' query_id = my_query,
#' date_range = c("2018-01-01", "2018-02-01"))
bwr_mentions_topsites <- function(project_id = NULL,
                                query_id = NULL,
                                querygrp_id = NULL,
                                date_range = c(Sys.Date()-31, Sys.Date()-1),
                                filters = NULL,
                                token = Sys.getenv("BW_TOKEN")) {
  url <- paste0("https://api.brandwatch.com/projects/",
                project_id,
                "/data/volume/topsites/queries")
  query <-  c(list(startDate = date_range[1],
                 endDate = date_range[2],
                 access_token = token,
                 queryId = query_id,
                 queryGroupId = querygrp_id),
              filters)

  r <- httr::GET(url,
                 query = query)
  httr::stop_for_status(r)

  # Parse the results and return
  con <- httr::content(r, "text")
  json <- jsonlite::fromJSON(con)
  json$results
}

#' Get a dataframe of top author data for a specified Brandwatch query or query group
#'
#' @param project_id
#' The project id in which the specified query is contained. Obtain a list of project IDs using bwr_get_projects().
#' @param query_id
#' The query ID you'd like to see author data for. Note that you can only specify a query or a query group, not both.
#' @param date_range
#' A character vector containing 2 date values in YYYY-mm-dd format. The first value is the beginning of your desired date range and the second value is the end of the date range.
#' @param querygrp_id
#' The query group ID you'd like to see total mentions for. Note that you can only specify a query or a query group, not both.
#' @param token
#' The authentication token, acquired using bwr_auth()
#' @param filters
#' (Optional) A list of key-value pairs to filter the mentions query by. Use the bwr_filters_get() function to find out all available filters.
#'
#' @return
#' Returns a data frame (including some list-columns) of the JSON results, showing information about the top authors related to your query.
#' @export
#'
#' @examples
#' my_project <- bwr_projects_get()$id[1]
#' my_query <- bwr_query_get(project_id = my_project)$id[1]
#' my_mentions <- bwr_mentions_topauthors(
#' project_id = my_project,
#' query_id = my_query,
#' date_range = c("2018-01-01", "2018-02-01"))
bwr_mentions_topauthors <- function(project_id = NULL,
                                  query_id = NULL,
                                  querygrp_id = NULL,
                                  date_range = c(Sys.Date()-31, Sys.Date()-1),
                                  filters = NULL,
                                  token = Sys.getenv("BW_TOKEN")) {
  url <- paste0("https://api.brandwatch.com/projects/",
                project_id,
                "/data/volume/topauthors/queries")
  query <-  c(list(startDate = date_range[1],
                 endDate = date_range[2],
                 access_token = token,
                 queryId = query_id,
                 queryGroupId = querygrp_id),
              filters)

  r <- httr::GET(url,
                 query = query)
  httr::stop_for_status(r)

  # Parse the results and return
  con <- httr::content(r, "text")
  json <- jsonlite::fromJSON(con)
  results <- json$results$data
  results

}





#' Get a dataframe of top Tweeter for a specified Brandwatch query or query group
#'
#' @param project_id
#' The project id in which the specified query is contained. Obtain a list of project IDs using bwr_get_projects().
#' @param query_id
#' The query ID you'd like to see tweeter data for. Note that you can only specify a query or a query group, not both.
#' @param date_range
#' A character vector containing 2 date values in YYYY-mm-dd format. The first value is the beginning of your desired date range and the second value is the end of the date range.
#' @param querygrp_id
#' The query group ID you'd like to see total mentions for. Note that you can only specify a query or a query group, not both.
#' @param token
#' The authentication token, acquired using bwr_auth()
#' @param filters
#' (Optional) A list of key-value pairs to filter the mentions query by. Use the bwr_filters_get() function to find out all available filters.
#'
#' @return
#' Returns a data frame (including some list-columns) of the JSON results, showing information about the top tweeters related to your query.
#' @export
#'
#' @examples
#' my_project <- bwr_projects_get()$id[1]
#' my_query <- bwr_query_get(project_id = my_project)$id[1]
#' my_mentions <- bwr_mentions_toptweeters(
#' project_id = my_project,
#' query_id = my_query,
#' date_range = c("2018-01-01", "2018-02-01"))
bwr_mentions_toptweeters <- function(project_id = NULL,
                                    query_id = NULL,
                                    querygrp_id = NULL,
                                    date_range = c(Sys.Date()-31, Sys.Date()-1),
                                    filters = NULL,
                                    token = Sys.getenv("BW_TOKEN")) {
  url <- paste0("https://api.brandwatch.com/projects/",
                project_id,
                "/data/volume/toptweeters/queries")
  query <-  c(list(startDate = date_range[1],
                 endDate = date_range[2],
                 access_token = token,
                 queryId = query_id,
                 queryGroupId = querygrp_id),
              filters)

  r <- httr::GET(url,
                 query = query)
  httr::stop_for_status(r)

  # Parse the results and return
  con <- httr::content(r, "text")
  json <- jsonlite::fromJSON(con)
  results <- json$results$data
  results

}


