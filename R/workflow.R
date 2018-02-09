
#' Get a list of workflows for the specified query and project
#'
#' Refer to https://developers.brandwatch.com/docs/retrieving-workflow for more information.
#'
#' @param token
#' The authentication token, acquired using bwr_auth()
#' @param project_id
#' The project id in which the specified workflow is contained. Obtain a list of project IDs using bwr_get_projects().
#' @return
#' Returns a list containing all results
#' @export
#'
#' @examples
#' my_project <- bwr_get_projects()$id[1]
#' my_workflow <- bwr_wf_get(project_id = my_project)
#'
bwr_wf_get <- function(token = Sys.getenv("BW_TOKEN"),
                             project_id = NULL) {
  url <- paste0("https://api.brandwatch.com/projects/",
                project_id,
                "/workflow")
  r <- httr::GET(url,
                 query = list(access_token = token))
  httr::stop_for_status(r)

  # Parse the results and return
  con <- httr::content(r, "text")
  json <- jsonlite::fromJSON(con)
  json
}


