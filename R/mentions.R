
#' Get a list of mentions for the specified query and project
#'
#' Returns a data frame containing any results found for the specified query ID.
#'
#' @param project_id Numeric.
#' The project id in which the specified query is contained.
#'   Obtain a list of project IDs using bwr_get_projects().
#' @param query_id Numeric
#' The id of the query you'd like to run.
#'   Obtain a list of queries for any specified project using bwr_get_queries().
#' @param date_range
#' A character vector containing 2 date values in YYYY-mm-dd format.
#' The first value is the beginning of your desired date range and the second value is the end of the date range.
#' @param page Numeric, optional.
#' The page number for which to return results.
#' This is needed only when R needs to iterate through multiple pages of results from the Brandwatch API.
#' It is recommended that you keep this value set as zero.
#' @param token
#' The authentication token, acquired using bwr_auth().
#'   This token will be automatically obtained from the user's
#'   environment variable 'BW_TOKEN' and does not need to be provided.
#' @param filters
#' (Optional) A list of key-value pairs to filter the mentions query by.
#' Use the bwr_filters_get() function to find out all available filters.
#' @param order_by String.
#' (Optional) The name of the metric you'd like to order your results by.
#' @param order_direction String.
#' Either 'asc' or 'desc', to specify ascending or descending order.
#' @return
#' Returns a data frame containing all results
#' @export
#'
#' @examples
#' \dontrun{my_project <- bwr_projects_get()$id[1]
#' my_query <- bwr_query_get(project_id = my_project)$id[1]
#' my_mentions <- bwr_mentions_get(project_id = my_project,
#'                                 query_id = my_query,
#'                                 filters = list(gender = 'female', sentiment = 'neutral'),
#'                                 order_by = 'sentiment', order_direction = 'asc',
#'                                 date_range = c('2018-01-01', '2018-02-01'))}
#'
#' @seealso \code{\link{bwr_auth}} to authenticate, \code{\link{bwr_projects_get}} for a list of project IDs, \code{\link{bwr_query_get}} for all queries available in the specified project.
#'   \code{\link{bwr_filters_get}} to get a list of available filters for the filters argument.
#'
bwr_mentions_get <- function(project_id = NULL, query_id = NULL, date_range = c(Sys.Date() - 31, Sys.Date() - 1), order_by = NULL, order_direction = NULL,
    filters = NULL, page = NULL, token = Sys.getenv("BW_TOKEN")) {


    # Check for valid arguments -----------------------------------------------
    if (length(token) != 1 || class(token) != "character")
        stop("Token object does not appear to be a character vector of length one. Please re-run bwr_auth() to obtain a token")

  if (is.null(project_id) || length(project_id) != 1 || !class(project_id) %in% c("character", "numeric", "integer"))
        stop("project_id must be a character or numeric vector of length one")
    if (is.null(query_id) || length(query_id) != 1 || !class(query_id) %in% c("character", "numeric", "integer"))
        stop("query_id must be a character or numeric vector of length one")
    if (!is.null(page) && class(page) != "numeric" && length(page) != 1)
        stop("Page must be either NULL or a single numeric vector of length one")
    if (length(date_range) != 2 || !class(date_range) %in% c("character", "Date"))
        stop("date_range must be a length 2 vector of either date or character class")

    print(paste("Reading page index: ", ifelse(is.null(page), 0, page)))

    url <- paste0("https://api.brandwatch.com/projects/", project_id, "/data/mentions")
    r <- httr::GET(url, query = c(list(queryId = query_id, startDate = date_range[1], endDate = date_range[2], pageSize = 5000, page = page,
        access_token = token), filters, orderBy = order_by, orderDirection = order_direction))
    httr::stop_for_status(r)

    # Parse the results and return
    con <- httr::content(r, "text")
    json <- jsonlite::fromJSON(con)
    results <- json$results

    # Deal with paging - if results >= 5000, check next page
    if (dim(results)[1] >= 5000) {
        results <- data.table::rbindlist(list(results, bwr_mentions_get(project_id = project_id, query_id = query_id, date_range = date_range,
            page = json$resultsPage + 1)), fill = TRUE)
    }

    results
}

#' Get the total mentions for a specified Brandwatch query or query group
#'
#' @param project_id
#' The project id in which the specified query is contained.
#' Obtain a list of project IDs using bwr_get_projects().
#' @param query_id
#' The query ID you'd like to see total mentions for.
#' Note that you can only specify a query or a query group, not both.
#' @param date_range
#' A character vector containing 2 date values in YYYY-mm-dd format.
#' The first value is the beginning of your desired date range and the second value is the end of the date range.
#' @param querygrp_id
#' The query group ID you'd like to see total mentions for.
#' Note that you can only specify a query or a query group, not both.
#' @param token
#' The authentication token, acquired using bwr_auth()
#' @param filters
#' (Optional) A list of key-value pairs to filter the mentions query by.
#' Use the bwr_filters_get() function to find out all available filters.
#' @return
#' Returns a numeric vector of length 1 with the results.
#' @export
#'
#' @examples
#' \dontrun{my_project <- bwr_projects_get()$id[1]
#' my_query <- bwr_queries_get(project_id = my_project)$id[1]
#' my_mentions <- bwr_mentions_total(
#' project_id = my_project,
#' query_id = my_query,
#' date_range = c('2018-01-01', '2018-02-01'))}
#'
bwr_mentions_total <- function(project_id = NULL, query_id = NULL, querygrp_id = NULL, filters = NULL, date_range = c(Sys.Date() - 31, Sys.Date() -
    1), token = Sys.getenv("BW_TOKEN")) {

    # Check for valid arguments -----------------------------------------------
    if (length(token) != 1 || class(token) != "character")
        stop("Token object does not appear to be a character vector of length one. Please re-run bwr_auth() to obtain a token")
    if (!is.null(query_id) && !is.null(querygrp_id)) stop("You cannot provide both a query_id and querygrp_id at the same time")
    if (is.null(query_id) && is.null(querygrp_id))
        stop("You must provide either a query_id or a querygrp_id")
    if (!is.null(query_id) || !is.null(querygrp_id)) {
        if (is.null(querygrp_id) && (length(query_id) != 1 || !class(query_id) %in% c("character", "numeric", "integer")))
            stop("query_id must be a character or numeric vector of length one")
        if (is.null(query_id) && (length(querygrp_id) != 1 || !class(querygrp_id) %in% c("character", "numeric", "integer")))
            stop("querygrp_id must be a character or numeric vector of length one")
    }
    if (is.null(project_id) || length(project_id) != 1 || !class(project_id) %in% c("character", "numeric", "integer"))
        stop("project_id must be a character or numeric vector of length one")
    if (length(date_range) != 2 || !class(date_range) %in% c("character", "Date"))
        stop("date_range must be a length 2 vector of either date or character class")


    url <- paste0("https://api.brandwatch.com/projects/", project_id, "/data/mentions/count")
    query <- c(list(startDate = date_range[1], endDate = date_range[2], access_token = token, queryId = query_id, queryGroupId = querygrp_id),
        filters)

    r <- httr::GET(url, query = query)
    httr::stop_for_status(r)

    # Parse the results and return
    con <- httr::content(r, "text")
    json <- jsonlite::fromJSON(con)
    json$mentionsCount
}


#' Get a dataframe of topic data for a specified Brandwatch query or query group
#'
#' @param project_id
#' The project id in which the specified query is contained.
#' Obtain a list of project IDs using bwr_get_projects().
#' @param query_id
#' The query ID you'd like to see total mentions for.
#' Note that you can only specify a query or a query group, not both.
#' @param date_range
#' A character vector containing 2 date values in YYYY-mm-dd format.
#' The first value is the beginning of your desired date range and the second value is the end of the date range.
#' @param querygrp_id
#' The query group ID you'd like to see total mentions for.
#' Note that you can only specify a query or a query group, not both.
#' @param token
#' The authentication token, acquired using bwr_auth()
#' @param filters
#' (Optional) A list of key-value pairs to filter the mentions query by.
#' Use the bwr_filters_get() function to find out all available filters.
#' @param order_by
#' Metric by which you wish to order the topics returned.
#' Can be either 'volume' (the number of Mentions the topic appears in)
#' or 'burst' (the rate that the topic has emerged over time).
#' @param limit
#' The maximum number of topics to be returned.
#' @param exclude_categories
#' You have the option to exclude Categories from the topics returned (this is often done in the Brandwatch Analytics UI).
#' @param exclude_tags
#' The authentication token, acquired using bwr_auth()
#' @return
#' Returns a data frame (including some list-columns) of the JSON results,
#' showing information about topics related to your query.
#' @export
#'
#' @examples
#' \dontrun{my_project <- bwr_projects_get()$id[1]
#' my_query <- bwr_query_get(project_id = my_project)$id[1]
#' my_mentions <- bwr_mentions_topics(project_id = my_project,
#'                                    query_id = my_query,
#'                                    date_range = c('2018-01-01', '2018-02-01'))}
bwr_mentions_topics <- function(project_id = NULL, query_id = NULL, querygrp_id = NULL, date_range = c(Sys.Date() - 31, Sys.Date() - 1),
    order_by = NULL, limit = NULL, exclude_categories = NULL, exclude_tags = NULL, filters = NULL, token = Sys.getenv("BW_TOKEN")) {

    # Check for valid arguments -----------------------------------------------
    if (length(token) != 1 || class(token) != "character")
        stop("Token object does not appear to be a character vector of length one. Please re-run bwr_auth() to obtain a token")
    if (!is.null(query_id) && !is.null(querygrp_id)) stop("You cannot provide both a query_id and querygrp_id at the same time")
    if (is.null(query_id) && is.null(querygrp_id))
        stop("You must provide either a query_id or a querygrp_id")
    if (!is.null(query_id) || !is.null(querygrp_id)) {
        if (is.null(querygrp_id) && (length(query_id) != 1 || !class(query_id) %in% c("character", "numeric", "integer")))
            stop("query_id must be a character or numeric vector of length one")
        if (is.null(query_id) && (length(querygrp_id) != 1 || !class(querygrp_id) %in% c("character", "numeric", "integer")))
            stop("querygrp_id must be a character or numeric vector of length one")
    }
    if (is.null(project_id) || length(project_id) != 1 || !class(project_id) %in% c("character", "numeric", "integer"))
        stop("project_id must be a character or numeric vector of length one")
    if (length(date_range) != 2 || !class(date_range) %in% c("character", "Date"))
        stop("date_range must be a length 2 vector of either date or character class")

    url <- paste0("https://api.brandwatch.com/projects/", project_id, "/data/volume/topics/queries")
    query <- c(list(startDate = date_range[1], endDate = date_range[2], access_token = token, queryId = query_id, queryGroupId = querygrp_id,
        orderBy = order_by, limit = limit, excludeCategories = exclude_categories, excludeTags = exclude_tags), filters)

    r <- httr::GET(url, query = query)
    httr::stop_for_status(r)

    # Parse the results and return
    con <- httr::content(r, "text")
    json <- jsonlite::fromJSON(con)
    json$topics
}




#' Get a dataframe of top site data for a specified Brandwatch query or query group
#'
#' @param project_id
#' The project id in which the specified query is contained.
#' Obtain a list of project IDs using bwr_get_projects().
#' @param query_id
#' The query ID you'd like to see total mentions for. Note that you can only specify a query or a query group, not both.
#' @param date_range
#' A character vector containing 2 date values in YYYY-mm-dd format.
#' The first value is the beginning of your desired date range and the second value is the end of the date range.
#' @param querygrp_id
#' The query group ID you'd like to see total mentions for.
#' Note that you can only specify a query or a query group, not both.
#' @param token
#' The authentication token, acquired using bwr_auth()
#' @param filters
#' (Optional) A list of key-value pairs to filter the mentions query by.
#' Use the bwr_filters_get() function to find out all available filters.
#'
#' @return
#' Returns a data frame (including some list-columns) of the JSON results,
#' showing information about the top sites related to your query.
#' @export
#'
#' @examples
#' \dontrun{my_project <- bwr_projects_get()$id[1]
#' my_query <- bwr_query_get(project_id = my_project)$id[1]
#' my_mentions <- bwr_mentions_topsites(project_id = my_project,
#'                                      query_id = my_query,
#'                                      date_range = c('2018-01-01', '2018-02-01'))}
bwr_mentions_topsites <- function(project_id = NULL, query_id = NULL, querygrp_id = NULL, date_range = c(Sys.Date() - 31, Sys.Date() - 1),
    filters = NULL, token = Sys.getenv("BW_TOKEN")) {

    # Check for valid arguments -----------------------------------------------
    if (length(token) != 1 || class(token) != "character")
        stop("Token object does not appear to be a character vector of length one. Please re-run bwr_auth() to obtain a token")
    if (!is.null(query_id) && !is.null(querygrp_id)) stop("You cannot provide both a query_id and querygrp_id at the same time")
    if (is.null(query_id) && is.null(querygrp_id))
        stop("You must provide either a query_id or a querygrp_id")
    if (!is.null(query_id) || !is.null(querygrp_id)) {
        if (is.null(querygrp_id) && (length(query_id) != 1 || !class(query_id) %in% c("character", "numeric", "integer")))
            stop("query_id must be a character or numeric vector of length one")
        if (is.null(query_id) && (length(querygrp_id) != 1 || !class(querygrp_id) %in% c("character", "numeric", "integer")))
            stop("querygrp_id must be a character or numeric vector of length one")
    }
    if (is.null(project_id) || length(project_id) != 1 || !class(project_id) %in% c("character", "numeric", "integer"))
        stop("project_id must be a character or numeric vector of length one")
    if (length(date_range) != 2 || !class(date_range) %in% c("character", "Date"))
        stop("date_range must be a length 2 vector of either date or character class")


    url <- paste0("https://api.brandwatch.com/projects/", project_id, "/data/volume/topsites/queries")
    query <- c(list(startDate = date_range[1], endDate = date_range[2], access_token = token, queryId = query_id, queryGroupId = querygrp_id),
        filters)

    r <- httr::GET(url, query = query)
    httr::stop_for_status(r)

    # Parse the results and return
    con <- httr::content(r, "text")
    json <- jsonlite::fromJSON(con)
    json$results
}

#' Get a dataframe of top author data for a specified Brandwatch query or query group
#'
#' @param project_id
#' The project id in which the specified query is contained.
#' Obtain a list of project IDs using bwr_get_projects().
#' @param query_id
#' The query ID you'd like to see author data for.
#' Note that you can only specify a query or a query group, not both.
#' @param date_range
#' A character vector containing 2 date values in YYYY-mm-dd format.
#' The first value is the beginning of your desired date range and the second value is the end of the date range.
#' @param querygrp_id
#' The query group ID you'd like to see total mentions for.
#' Note that you can only specify a query or a query group, not both.
#' @param token
#' The authentication token, acquired using bwr_auth()
#' @param filters
#' (Optional) A list of key-value pairs to filter the mentions query by.
#' Use the bwr_filters_get() function to find out all available filters.
#'
#' @return
#' Returns a data frame (including some list-columns) of the JSON results,
#' showing information about the top authors related to your query.
#' @export
#'
#' @examples
#' \dontrun{my_project <- bwr_projects_get()$id[1]
#' my_query <- bwr_query_get(project_id = my_project)$id[1]
#' my_mentions <- bwr_mentions_topauthors(project_id = my_project,
#'                                        query_id = my_query,
#'                                        date_range = c('2018-01-01', '2018-02-01'))}
bwr_mentions_topauthors <- function(project_id = NULL, query_id = NULL, querygrp_id = NULL, date_range = c(Sys.Date() - 31, Sys.Date() -
    1), filters = NULL, token = Sys.getenv("BW_TOKEN")) {
    # Check for valid arguments -----------------------------------------------
    if (length(token) != 1 || class(token) != "character")
        stop("Token object does not appear to be a character vector of length one. Please re-run bwr_auth() to obtain a token")
    if (!is.null(query_id) && !is.null(querygrp_id)) stop("You cannot provide both a query_id and querygrp_id at the same time")

    if (is.null(query_id) && is.null(querygrp_id))
        stop("You must provide either a query_id or a querygrp_id")
    if (!is.null(query_id) || !is.null(querygrp_id)) {
        if (is.null(querygrp_id) && (length(query_id) != 1 || !class(query_id) %in% c("character", "numeric", "integer")))
            stop("query_id must be a character or numeric vector of length one")
        if (is.null(query_id) && (length(querygrp_id) != 1 || !class(querygrp_id) %in% c("character", "numeric", "integer")))
            stop("querygrp_id must be a character or numeric vector of length one")
    }
    if (is.null(project_id) || length(project_id) != 1 || !class(project_id) %in% c("character", "numeric", "integer"))
        stop("project_id must be a character or numeric vector of length one")
    if (length(date_range) != 2 || !class(date_range) %in% c("character", "Date"))
        stop("date_range must be a length 2 vector of either date or character class")


    url <- paste0("https://api.brandwatch.com/projects/", project_id, "/data/volume/topauthors/queries")
    query <- c(list(startDate = date_range[1], endDate = date_range[2], access_token = token, queryId = query_id, queryGroupId = querygrp_id),
        filters)

    r <- httr::GET(url, query = query)
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
#' A character vector containing 2 date values in YYYY-mm-dd format.
#' The first value is the beginning of your desired date range and the second value is the end of the date range.
#' @param querygrp_id
#' The query group ID you'd like to see total mentions for.
#' Note that you can only specify a query or a query group, not both.
#' @param token
#' The authentication token, acquired using bwr_auth()
#' @param filters
#' (Optional) A list of key-value pairs to filter the mentions query by.
#' Use the bwr_filters_get() function to find out all available filters.
#'
#' @return
#' Returns a data frame (including some list-columns) of the JSON results,
#' showing information about the top tweeters related to your query.
#' @export
#'
#' @examples
#' \dontrun{my_project <- bwr_projects_get()$id[1]
#' my_query <- bwr_query_get(project_id = my_project)$id[1]
#' my_mentions <- bwr_mentions_toptweeters(project_id = my_project,
#'                                         query_id = my_query,
#'                                         date_range = c('2018-01-01', '2018-02-01'))}
bwr_mentions_toptweeters <- function(project_id = NULL, query_id = NULL, querygrp_id = NULL, date_range = c(Sys.Date() - 31, Sys.Date() -1),
                                     filters = NULL, token = Sys.getenv("BW_TOKEN")) {

    # Check for valid arguments -----------------------------------------------
    if (length(token) != 1 || class(token) != "character")
        stop("Token object does not appear to be a character vector of length one. Please re-run bwr_auth() to obtain a token")
    if (!is.null(query_id) && !is.null(querygrp_id)) stop("You cannot provide both a query_id and querygrp_id at the same time")
    if (is.null(query_id) && is.null(querygrp_id))
        stop("You must provide either a query_id or a querygrp_id")
    if (!is.null(query_id) || !is.null(querygrp_id)) {
        if (is.null(querygrp_id) && (length(query_id) != 1 || !class(query_id) %in% c("character", "numeric", "integer")))
            stop("query_id must be a character or numeric vector of length one")
        if (is.null(query_id) && (length(querygrp_id) != 1 || !class(querygrp_id) %in% c("character", "numeric", "integer")))
            stop("querygrp_id must be a character or numeric vector of length one")
    }
    if (is.null(project_id) || length(project_id) != 1 || !class(project_id) %in% c("character", "numeric", "integer"))
        stop("project_id must be a character or numeric vector of length one")
    if (length(date_range) != 2 || !class(date_range) %in% c("character", "Date"))
        stop("date_range must be a length 2 vector of either date or character class")

    url <- paste0("https://api.brandwatch.com/projects/", project_id, "/data/volume/toptweeters/queries")
    query <- c(list(startDate = date_range[1], endDate = date_range[2], access_token = token, queryId = query_id, queryGroupId = querygrp_id),
        filters)

    r <- httr::GET(url, query = query)
    httr::stop_for_status(r)

    # Parse the results and return
    con <- httr::content(r, "text")
    json <- jsonlite::fromJSON(con)
    results <- json$results$data
    results

}


#' Get a dataframe of chart-ready data for a specified Brandwatch query or query group
#'
#' For more information, please refer to https://developers.brandwatch.com/docs/chart-dimensions-and-aggregates
#'
#' @param project_id
#' The project id in which the specified query is contained. Obtain a list of project IDs using bwr_get_projects().
#' @param query_id
#' The query ID you'd like to see tweeter data for. Note that you can only specify a query or a query group, not both.
#' @param date_range
#' A character vector containing 2 date values in YYYY-mm-dd format.
#' The first value is the beginning of your desired date range and the second value is the end of the date range.
#' @param querygrp_id
#' The query group ID you'd like to see total mentions for.
#' Note that you can only specify a query or a query group, not both.
#' @param token
#' The authentication token, acquired using bwr_auth()
#' @param filters
#' (Optional) A list of key-value pairs to filter the mentions query by.
#' Use the bwr_filters_get() function to find out all available filters.
#' @param aggregate
#' The numeric variable which you'd like to return results for (e.g. 'volume')
#' @param dimension1
#' The first dimension which you'd like to split your data by (e.g. 'months')
#'
#' @param dimension2
#' The second dimension which you'd like to split your data by (e.g. 'countries')
#'
#' @return
#' Returns a data frame (including some list-columns) of the JSON results,
#' aggregated according to your 2 dimensions.
#' @export
#'
#' @examples
#' \dontrun{my_project <- bwr_projects_get()$id[1]
#' my_query <- bwr_query_get(project_id = my_project)$id[1]
#' my_mentions_chart <- bwr_mentions_chart(project_id = my_project,
#'                                         query_id = my_query,
#'                                         date_range = c('2018-01-01', '2018-02-01'),
#'                                         aggregate = "month",
#'                                         dimension1 = "sentiment",
#'                                         dimension2 = "volume")}
bwr_mentions_chart <- function(project_id = NULL,
                               query_id = NULL,
                               querygrp_id = NULL,
                               date_range = c(Sys.Date() - 31, Sys.Date() -1),
                               aggregate = NULL,
                               dimension1 = NULL,
                               dimension2 = NULL,
                               filters = NULL,
                               token = Sys.getenv("BW_TOKEN")) {

  # Check for valid arguments -----------------------------------------------
  if (length(token) != 1 || class(token) != "character")
    stop("Token object does not appear to be a character vector of length one. Please re-run bwr_auth() to obtain a token")
  if (!is.null(query_id) && !is.null(querygrp_id)) stop("You cannot provide both a query_id and querygrp_id at the same time")
  if (is.null(query_id) && is.null(querygrp_id))
    stop("You must provide either a query_id or a querygrp_id")
  if (!is.null(query_id) || !is.null(querygrp_id)) {
    if (is.null(querygrp_id) && (length(query_id) != 1 || !class(query_id) %in% c("character", "numeric", "integer")))
      stop("query_id must be a character or numeric vector of length one")
    if (is.null(query_id) && (length(querygrp_id) != 1 || !class(querygrp_id) %in% c("character", "numeric", "integer")))
      stop("querygrp_id must be a character or numeric vector of length one")
  }
  if (is.null(project_id) || length(project_id) != 1 || !class(project_id) %in% c("character", "numeric", "integer"))
    stop("project_id must be a character or numeric vector of length one")
  if (is.null(aggregate) || length(aggregate) != 1 || class(aggregate) != "character")
    stop("aggregate must be a character vector of length one")
  if (is.null(dimension1) || length(dimension1) != 1 || class(dimension1) != "character")
    stop("dimension1 must be a character vector of length one")
  if (is.null(dimension2) || length(dimension2) != 1 || class(dimension2) != "character")
    stop("dimension2 must be a character vector of length one")
  if (length(date_range) != 2 || !class(date_range) %in% c("character", "Date"))
    stop("date_range must be a length 2 vector of either date or character class")

  url <- paste0("https://api.brandwatch.com/projects/", project_id,
                "/data/", aggregate, "/", dimension1, "/", dimension2)
  query <- c(list(startDate = date_range[1], endDate = date_range[2], access_token = token, queryId = query_id, queryGroupId = querygrp_id),
             filters)

  r <- httr::GET(url, query = query)
  httr::stop_for_status(r)

  # Parse the results and return
  con <- httr::content(r, "text")
  json <- jsonlite::fromJSON(con)

  result_list <- lapply(seq_along(json$results$name), function(x) {
    data <- json$results$values[[x]]
    data[json$dimension1] <- json$results$name[x]
    data <- data[,c('id', 'name', 'value', json$dimension1)]
    names(data)[2] <- json$dimension2
    data
  })

  results <- do.call("rbind", result_list)
  results
}
