#' Get a data frame of available rules for a given project
#'
#' See the Brandwatch documentation for more information at https://developers.brandwatch.com/docs/retrieving-rules.
#'
#' @param token
#' The auth token for this user, which should be generated into an environment variable using bwr_auth()
#' @param project_id
#' The project id you'd like to return all available rules for. Obtain a data frame of projects using bwr_get_projects()
#' @return
#' Returns a dataframe of rules matching the project. Due to the structure of the API response, some of the dataframe columns are list-columns (i.e. nested dataframes).
#' @export
#'
#' @examples
#' my_rules <- bwr_rules_get(project_id = 12334534)
bwr_rule_get <- function(token = Sys.getenv("BW_TOKEN"),
                          project_id = NULL) {
  url <- paste0("https://api.brandwatch.com/projects/",
                project_id,
                "/rules")
  r <- httr::GET(url,
                 query = list(access_token = token))
  httr::stop_for_status(r)

  # Parse the results and return
  con <- httr::content(r, "text")
  json <- jsonlite::fromJSON(con)
  results <- json$results
  results
}


#' Delete a specified Brandwatch rule
#'
#' @param token
#' The authentication token, obtained using bwr_auth()
#' @param project_id
#' The project ID in which the target rule can be found. Obtain a data frame of project IDs using bwr_projects_get().
#' @param rule_id
#' The ID of the rule which you'd like to delete. Obtain a list of rule IDs using bwr_rule_get().
#'
#' @return
#' Returns a list of the JSON response.
#' @export
#'
#' @examples
#' bwr_rule_delete(project_id = 122445, rule_id = 23432424)
bwr_rule_delete <- function(token = Sys.getenv("BW_TOKEN"),
                             project_id,
                             rule_id ) {
  url <- paste0("https://api.brandwatch.com/projects/", project_id, "/rules/", rule_id, "?access_token=", token)
  r <- httr::DELETE(url)
  httr::stop_for_status(r)

  # Parse the results and return
  con <- httr::content(r, "text")
  json <- jsonlite::fromJSON(con)
  base::message(paste0("Deleted rule.\n\tProject: ", project_id,
                       "\n\trule name: ", json$name,
                       "\n\trule ID: ", json$id))
  json
}
