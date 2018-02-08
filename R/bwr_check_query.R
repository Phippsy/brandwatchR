
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
#' any_issues <- bwr_check_query(query = list(query = "at_mentions:huey", language = "en"))
bwr_check_query <- function(token = Sys.getenv("BW_TOKEN"),
                             query = list() ) {
  library(jsonlite)
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
#' bwr_upload_query(project_id = 12423432, samplePercent = 50, includedTerms = "at_mentions:mickeymouse", name = "Sample API query")
bwr_upload_query <- function(token = Sys.getenv("BW_TOKEN"),
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
    print(i)
    query[[i]] <- unbox(query[[i]])
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
  json
}
