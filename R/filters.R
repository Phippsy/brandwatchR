#' Get a data frame of available filters for the Brandwatch API
#'
#' See the Brandwatch documentation for more information at https://developers.brandwatch.com/docs/available-filters
#'
#' @param token
#' The auth token for this user, which should be generated into an environment variable using bwr_auth()

#' @return
#' Returns a dataframe of available filters
#' @export
#'
#' @examples
#' my_queries <- bwr_query_get(project_id = 12334534)
bwr_filters_get <- function(token = Sys.getenv("BW_TOKEN")) {
  url <- "https://api.brandwatch.com/filters/"
  r <- httr::GET(url,
                 query = list(access_token = token))
  httr::stop_for_status(r)

  # Parse the results and return
  con <- httr::content(r, "text")
  json <- jsonlite::fromJSON(con)
  results <- data.frame(metric = names(json))

  metrics <- bwr_metrics_get()

  results <- merge(results, metrics, by = "metric", all = TRUE)
  results
}


#' Get a data frame of possible metric values for the Brandwatch API
#'
#' When examining parameters which can be used to filter a metric call, some parameters have a limited set of acceptable values. You can find out the current acceptable values using this function.
#'
#' See the Brandwatch documentation for more information at https://developers.brandwatch.com/docs/available-filters
#'
#' @param token
#' The auth token for this user, which should be generated into an environment variable using bwr_auth()

#' @return
#' Returns a dataframe of available filters
#' @export
#'
#' @examples
#' my_queries <- bwr_metrics_get(project_id = 12334534)
bwr_metrics_get <- function(token = Sys.getenv("BW_TOKEN")) {
  url <- "https://api.brandwatch.com/metrics/"
  r <- httr::GET(url,
                 query = list(access_token = token))
  httr::stop_for_status(r)

  # Parse the results and return
  con <- httr::content(r, "text")
  json <- jsonlite::fromJSON(con)
  json <- lapply(json, paste, collapse = ",")
  results <- utils::stack(json)
  results <- results[,c('ind', 'values')]
  names(results) <- c("metric", "accepted values")
  results$metric <- gsub("s$", "", results$metric)
  results
}


add_filters <- function(x) {
  query[[x]] <- filter[[x]]
}
