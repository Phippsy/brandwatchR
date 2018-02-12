#' Get a data frame of Brandwatch query groups in the specified project
#'
#' @param token
#' The authentication token, obtained using bwr_auth()
#' @param project_id
#' The project ID whose query groups you'd like to return
#'
#' @return
#' Returns a data frame of the query groups for the given project, if any exist.
#' @export
#'
#' @examples
#' \dontrun{groups <- bwr_querygrp_get(project_id = 21343242)}
bwr_querygrp_get <- function(token = Sys.getenv("BW_TOKEN"), project_id = NULL) {

    # Check for valid arguments -----------------------------------------------
    if (length(token) != 1 || class(token) != "character")
        stop("Token object does not appear to be a character vector of length one. Please re-run bwr_auth() to obtain a token")
    if (is.null(project_id) || length(project_id) != 1 || !class(project_id) %in% c("character", "numeric"))
        stop("project_id must be a character or numeric vector of length one")


    url <- paste0("https://api.brandwatch.com/projects/", project_id, "/querygroups")
    r <- httr::GET(url, query = list(access_token = token))
    httr::stop_for_status(r)

    # Parse the results and return
    con <- httr::content(r, "text")
    json <- jsonlite::fromJSON(con)
    results <- json$results
    results
}


#' Delete a specified Brandwatch query group
#'
#' @param token
#' The API authentication token, obtained using bwr_auth()
#' @param project_id
#' The project ID in which your target query group can be found.
#' @param group_id
#' The query group ID which you'd like to delete. Find out the list of query groups in a given project using bwr_querygrp_get().
#'
#' @return
#' Returns a list of the JSON response from the server.
#' @export
#'
#' @examples
#' \dontrun{kill <- bwr_querygrp_delete(project_id = 234234234, group_id = 23423423)}
bwr_querygrp_delete <- function(token = Sys.getenv("BW_TOKEN"), project_id = NULL, group_id = NULL) {
    # Check for valid arguments -----------------------------------------------
    if (length(token) != 1 || class(token) != "character")
        stop("Token object does not appear to be a character vector of length one. Please re-run bwr_auth() to obtain a token")
    if (is.null(project_id) || length(project_id) != 1 || !class(project_id) %in% c("character", "numeric"))
        stop("project_id must be a character or numeric vector of length one")
    if (is.null(group_id) || length(group_id) != 1 || !class(group_id) %in% c("character", "numeric"))
        stop("group_id must be a character or numeric vector of length one")

    url <- paste0("https://api.brandwatch.com/projects/", project_id, "/querygroups/", group_id)
    r <- httr::DELETE(url, query = list(access_token = token))
    httr::stop_for_status(r)

    # Parse the results and return
    con <- httr::content(r, "text")
    json <- jsonlite::fromJSON(con)

    # Confirm deletion
    base::message(paste0("Deleted query group.\n\tProject: ", project_id, "\n\tQuery Group name: ", json$name, "\n\tQuery Group ID: ", json$id))

    json
}
