#' Get Brandwatch project data
#'
#' @param token
#' The Brandwatch token, which will be automatically available via environment arguments provided you run bwr_auth() prior to any other functions.
#' @param target
#' (Optional) the target for your query. If not specified, results will include all available projects for the authenticated user and all available data for each project.
#' If "summary", results will be a more simplified list of projects and excludes richer data such as query links.
#'
#'
#' @return
#' Returns a data frame of project data.
#' @export
#'
#' @examples
#' my_projects <- bwr_get_projects(target = "summary")
#'
#' Read the Brandwatch documentation for this API call at https://developers.brandwatch.com/docs/retrieving-projects
bwr_get_projects <- function(token = Sys.getenv("BW_TOKEN"),
                             target = NULL ) {
  url <- paste0("https://api.brandwatch.com/projects/",
                target)
  r <- httr::GET(url,
                 query = list(access_token = token))
  httr::stop_for_status(r)

  # Parse the results and return
  con <- httr::content(r, "text")
  json <- jsonlite::fromJSON(con)
  results <- json$results
  results
}
