#' Get a data frame of available queries for a given project
#'
#' See the Brandwatch documentation for more information at https://developers.brandwatch.com/docs/retrieving-queries.
#'
#' @param token
#' The auth token for this user, which should be generated into an environment variable using bwr_auth()
#' @param project_id
#' The project id you'd like to return all available queries for. Obtain a data frame of projects using bwr_get_projects()
#' @param type
#' (Optional) The type of query you'd like to return. If not specified, the API will return all available queries for the given project.
#' Currently, results don't seem to be affected by the type parameter - the API appears to return full results in all cases.
#'
#' @return
#' Returns a dataframe of queries matching the project.
#' @export
#'
#' @examples
#' my_queries <- bwr_get_queries(project_id = 12334534)
bwr_get_queries <- function(token = Sys.getenv("BW_TOKEN"),
                             project_id = NULL,
                            type = NULL) {
  url <- paste0("https://api.brandwatch.com/projects/",
                project_id,
                "/queries/summary")
  r <- httr::GET(url,
                 query = list(access_token = token,
                              type = type))
  httr::stop_for_status(r)

  # Parse the results and return
  con <- httr::content(r, "text")
  json <- jsonlite::fromJSON(con)
  results <- json$results
  results
}
