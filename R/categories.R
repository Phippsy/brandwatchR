
#' Get a data frame of available categories for a given project
#'
#' See the Brandwatch documentation for more information
#' at https://developers.brandwatch.com/docs/retrieving-categories.
#'
#' @param token
#' The auth token for this user, which should be generated
#' into an environment variable using bwr_auth()
#' @param project_id
#' The project id you'd like to return all available categories for.
#' Obtain a data frame of projects using bwr_get_projects()
#' @return
#' Returns a dataframe of categories matching the project.
#' @export
#'
#' @examples
#' \dontrun{my_categories <- bwr_cat_get(project_id = 12334534))}
bwr_cat_get <- function(project_id = NULL, token = Sys.getenv("BW_TOKEN")) {
    if (length(token) != 1 || class(token) != "character")
        stop("Token object does not appear to be a character vector of length one. Please re-run bwr_auth() to obtain a token")
    if (is.null(project_id))
        stop("Please supply a valid project ID")
    url <- paste0("https://api.brandwatch.com/projects/", project_id, "/categories")
    r <- httr::GET(url, query = list(access_token = token))
    httr::stop_for_status(r)

    # Parse the results and return
    con <- httr::content(r, "text")
    json <- jsonlite::fromJSON(con)
    results <- json$results
    results
}

#' Upload a new category to Brandwatch
#'
#' Refer to https://developers.brandwatch.com/docs/creating-categories for
#' more information. Unless otherwise specified, provide a single string argument.
#'
#' @param token
#' The authentication token, obtained using bwr_auth()
#' @param project_id
#' The project ID into which you'd like to insert a category
#' @param name
#' The name of the category
#' @param children
#' A list of lists (sorry!), each list defining the key-value pairs for name (mandatory) and
#' id (optional) of each child subcategory.
#'
#' @return
#' Returns a list containing the JSON response from the server.
#' @export
#'
#' @examples
#' \dontrun{bwr_cat_create(project_id = 12423432,
#'                name = 'Quick test cat',
#'                children = list(list(name = 'category one', id = 234234),
#'                  list(name = 'category 2')))}
bwr_cat_create <- function(project_id = NULL, name = NULL, children = NULL, token = Sys.getenv("BW_TOKEN")) {

    # Check correct args ------------------------------------------------------
    if (length(token) != 1 || class(token) != "character")
        stop("Token object does not appear to be a character vector of length one. Please re-run bwr_auth() to obtain a token")
    if (is.null(project_id))
        stop("Please supply a valid project ID")
    if (is.null(name))
        stop("Please supply a valid name for the category")


    # Prepare submissions for API JSON format ---------------------------------
    if (!is.null(children)) {
        # Unbox any children
        prep_json <- function(item) {
            lapply(item, jsonlite::unbox)
        }
        children <- lapply(children, prep_json)
        cat_json <- (list(name = jsonlite::unbox(name), children = children))

        # Specify multiple = true in case of multiple children
        if (length(children) > 1) {
            cat_json$multiple <- jsonlite::unbox(TRUE)
        }
    } else {
        cat_json <- list(name = jsonlite::unbox(name))
    }


    # Make the request
    url <- paste0("https://api.brandwatch.com/projects/", project_id, "/categories/?access_token=", token)
    ready_json <- jsonlite::toJSON(cat_json)
    r <- httr::POST(url, httr::accept_json(), httr::add_headers(`Content-Type` = "application/json"), body = cat_json, encode = "json")
    httr::stop_for_status(r)

    # Parse the results and return
    con <- httr::content(r, "text")
    json <- jsonlite::fromJSON(con)

    base::message(paste0("Created new category.\n\tProject: ", project_id, "\n\tcategory name: ", json$name, "\n\tcategory ID: ", json$id))
    json
}
