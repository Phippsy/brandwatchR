
#' Get a data frame of available tags for a given project
#'
#' See the Brandwatch documentation for more information at https://developers.brandwatch.com/docs/retrieving-tags.
#'
#' @param token
#' The auth token for this user, which should be generated into an environment variable using bwr_auth()
#' @param project_id
#' The project id you'd like to return all available tags for. Obtain a data frame of projects using bwr_get_projects()
#' @return
#' Returns a dataframe of tags matching the project.
#' @export
#'
#' @examples
#' \dontrun{my_tags <- bwr_tag_get(project_id = 12334534)}
bwr_tag_get <- function(project_id = NULL, token = Sys.getenv("BW_TOKEN")) {

    # Check for valid arguments -----------------------------------------------
    if (length(token) != 1 || class(token) != "character")
        stop("Token object does not appear to be a character vector of length one. Please re-run bwr_auth() to obtain a token")
    if (is.null(project_id) || length(project_id) != 1 || !class(project_id) %in% c("character", "numeric"))
        stop("project_id must be a character or numeric vector of length one")

    url <- paste0("https://api.brandwatch.com/projects/", project_id, "/tags")
    r <- httr::GET(url, query = list(access_token = token))
    httr::stop_for_status(r)

    # Parse the results and return
    con <- httr::content(r, "text")
    json <- jsonlite::fromJSON(con)
    results <- json$results
    results
}

#' Upload a new tag to Brandwatch
#'
#' Refer to https://developers.brandwatch.com/docs/creating-tags for more information. Unless otherwise specified, provide a single string argument.
#'
#' @param token
#' The authentication token, obtained using bwr_auth()
#' @param project_id
#' The project ID into which you'd like to insert a tag
#' @param name
#' The name of the tag
#'
#' @return
#' Returns a list containing the JSON response from the server.
#' @export
#'
#' @examples
#' \dontrun{bwr_tag_create(project_id = 12423432, name = 'Quick test tag')}
bwr_tag_create <- function(project_id = NULL, name = NULL, token = Sys.getenv("BW_TOKEN")) {

    # Check for valid arguments -----------------------------------------------
    if (length(token) != 1 || class(token) != "character")
        stop("Token object does not appear to be a character vector of length one. Please re-run bwr_auth() to obtain a token")
    if (is.null(project_id) || length(project_id) != 1 || !class(project_id) %in% c("character", "numeric"))
        stop("project_id must be a character or numeric vector of length one")
    if (is.null(name) || length(name) != 1 || !class(name) %in% c("character"))
        stop("name must be a character vector of length one")


    # Make the request
    url <- paste0("https://api.brandwatch.com/projects/", project_id, "/tags/?access_token=", token)
    tag_json <- jsonlite::toJSON(list(name = jsonlite::unbox(name)))
    r <- httr::POST(url, httr::accept_json(), httr::add_headers(`Content-Type` = "application/json"), body = tag_json, encode = "json")
    httr::stop_for_status(r)

    # Parse the results and return
    con <- httr::content(r, "text")
    json <- jsonlite::fromJSON(con)

    base::message(paste0("Created new tag.\n\tProject: ", project_id, "\n\ttag name: ", json$name, "\n\ttag ID: ", json$id))
    json


}

#' Delete a specified Brandwatch tag
#'
#' @param token
#' The authentication token, obtained using bwr_auth()
#' @param project_id
#' The project ID in which the target tag can be found. Obtain a data frame of project IDs using bwr_projects_get().
#' @param tag_id
#' The ID of the tag which you'd like to delete. Obtain a list of tag IDs using bwr_tag_get().
#'
#' @return
#' Returns a list of the JSON response.
#' @export
#'
#' @examples
#' \dontrun{bwr_tag_delete(project_id = 122445, tag_id = 23432424)}
bwr_tag_delete <- function(project_id, tag_id, token = Sys.getenv("BW_TOKEN")) {
    # Check for valid arguments -----------------------------------------------
    if (length(token) != 1 || class(token) != "character")
        stop("Token object does not appear to be a character vector of length one. Please re-run bwr_auth() to obtain a token")
    if (is.null(project_id) || length(project_id) != 1 || !class(project_id) %in% c("character", "numeric"))
        stop("project_id must be a character or numeric vector of length one")
    if (is.null(tag_id) || length(tag_id) != 1 || !class(tag_id) %in% c("character", "numeric"))
        stop("tag_id must be a character or numeric vector of length one")

    url <- paste0("https://api.brandwatch.com/projects/", project_id, "/tags/", tag_id, "?access_token=", token)
    r <- httr::DELETE(url)
    httr::stop_for_status(r)

    # Parse the results and return
    con <- httr::content(r, "text")
    json <- jsonlite::fromJSON(con)
    base::message(paste0("Deleted tag.\n\tProject: ", project_id, "\n\ttag name: ", json$name, "\n\ttag ID: ", json$id))
    json
}
