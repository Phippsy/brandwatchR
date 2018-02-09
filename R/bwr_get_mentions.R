
#' Get a list of mentions for the specified query and project
#'
#' @param token
#' The authentication token, acquired using bwr_auth()
#' @param project_id
#' The project id in which the specified query is contained. Obtain a list of project IDs using bwr_get_projects().
#' @param query_id
#' The id of the query you'd like to run. Obtain a list of queries for any specified project using bwr_get_queries().
#' @param date_range
#' A character vector containing 2 date values in YYYY-mm-dd format. The first value is the beginning of your desired date range and the second value is the end of the date range.
#' @param page
#' (Optional) The page number for which to return results. This is needed only when R needs to iterate through multiple pages of results from the Brandwatch API. It is recommended that you keep this value set as zero.
#'
#' @return
#' Returns a data frame containing all results
#' @export
#'
#' @examples
#' my_project <- bwr_get_projects()$id[1]
#' my_query <- bwr_get_queries(project_id = my_project)$id[1]
#' my_mentions <- bwr_get_mentions(
#' project_id = my_project,
#' query_id = my_query,
#' date_range = c("2018-01-01", "2018-02-01"))
#'
bwr_get_mentions <- function(token = Sys.getenv("BW_TOKEN"),
                             project_id = NULL,
                            query_id = NULL,
                            date_range = c(Sys.Date()-31, Sys.Date()-1),
                            page = NULL) {
  print(paste("page = ", page))
  url <- paste0("https://api.brandwatch.com/projects/",
                project_id,
                "/data/mentions")
  r <- httr::GET(url,
                 query = list(queryId = query_id,
                              startDate = date_range[1],
                              endDate = date_range[2],
                              pageSize = 5000,
                              page = page,
                              access_token = token))
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


