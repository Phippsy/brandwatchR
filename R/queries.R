
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
#' my_queries <- bwr_query_get(project_id = 12334534)
bwr_query_get <- function(token = Sys.getenv("BW_TOKEN"),
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

#' Validate a Brandwatch query
#'
#' @param token
#' The authentication token, acquired using bwr_auth()
#' @param query
#' A list containing named parameters: query and language.
#'
#' @return
#' Returns a list containing any errors or issues, if found
#' @export
#'
#' @examples
#' any_issues <- bwr_query_check(query = list(query = "at_mentions:huey", language = "en"))
bwr_query_check <- function(token = Sys.getenv("BW_TOKEN"),
                            query = list() ) {
  url <- paste0("https://api.brandwatch.com/query-validation/")
  query$access_token <- token
  r <- httr::GET(url,
                 query = query)
  httr::stop_for_status(r)

  # Parse the results and return
  con <- httr::content(r, "text")
  json <- jsonlite::fromJSON(con)
  json
}

#' Upload a new query to Brandwatch
#'
#' Refer to https://developers.brandwatch.com/docs/creating-queries for more information. Unless otherwise specified, provide a single string argument.
#'
#' @param token
#' The authentication token, obtained using bwr_auth()
#' @param project_id
#' The project ID into which you'd like to insert a query
#' @param type
#' The type of query (e.g. 'search string')
#' @param languageAgnostic
#' (Boolean) Is the query language agnostic?
#' @param samplePercent
#' (Numeric) The sample percent for the given query
#' @param languages
#' Which languages to include
#' @param includedTerms
#' The query syntax to be included.
#' @param name
#' The name of the query
#' @param description
#' The description of the query
#' @param industry
#' The industry classification
#'
#' @return
#' Returns a list containing the JSON response from the server.
#' @export
#'
#' @examples
#' bwr_query_create(project_id = 12423432, samplePercent = 50, includedTerms = "at_mentions:mickeymouse", name = "Sample API query")
bwr_query_create <- function(token = Sys.getenv("BW_TOKEN"),
                             project_id = NULL,
                             type = "search string",
                             languageAgnostic = FALSE,
                             samplePercent = 100,
                             languages = "en",
                             includedTerms = NULL,
                             name = NULL,
                             description = "My API query",
                             industry = "general"
) {

  query <- list(
    type = type, languageAgnostic = languageAgnostic, samplePercent = samplePercent, languages = languages, includedTerms = includedTerms,
    name = name, description = description, industry = industry
  )
  # Prep the query parameters for conversion to JSON
  for ( i in c("type", "languageAgnostic", "samplePercent", "name", "description", "industry")) {
    query[[i]] <- jsonlite::unbox(query[[i]])
  }

  # Make the request
  url <- paste0("https://api.brandwatch.com/projects/", project_id, "/queries/?access_token=", token)
  query_json <- jsonlite::toJSON(query)
  r <- httr::POST(url,
                  accept_json(),
                  add_headers("Content-Type" = "application/json"),
                  body = query_json,
                  encode = "json")
  httr::stop_for_status(r)

  # Parse the results and return
  con <- httr::content(r, "text")
  json <- jsonlite::fromJSON(con)

  base::message(paste0("Created new query.\n\tProject: ", project_id,
                       "\n\tQuery name: ", json$name,
                       "\n\tQuery ID: ", json$id))
  json


}

#' Delete a specified Brandwatch query
#'
#' @param token
#' The authentication token, obtained using bwr_auth()
#' @param project_id
#' The project ID in which the target query can be found. Obtain a data frame of project IDs using bwr_projects_get().
#' @param query_id
#' The ID of the query which you'd like to delete. Obtain a list of query IDs using bwr_query_get().
#'
#' @return
#' Returns a list of the JSON response.
#' @export
#'
#' @examples
#' bwr_query_delete(project_id = 122445, query_id = 23432424)
bwr_query_delete <- function(token = Sys.getenv("BW_TOKEN"),
                             project_id,
                             query_id ) {
  url <- paste0("https://api.brandwatch.com/projects/", project_id, "/queries/", query_id)
  query$access_token <- token
  r <- httr::DELETE(url,
                 query = query)
  httr::stop_for_status(r)

  # Parse the results and return
  con <- httr::content(r, "text")
  json <- jsonlite::fromJSON(con)
  base::message(paste0("Deleted query.\n\tProject: ", project_id,
                       "\n\tQuery name: ", json$name,
                       "\n\tQuery ID: ", json$id))
  json
}
