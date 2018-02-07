#' Authenticated with the Brandwatch API
#'
#' @param un
#' Your Brandwatch Username
#' @param pw
#' Your Brandwatch Password
#' @param refresh
#' (Boolean) Should the token be refreshed? If no, a token will be read from a locally cached file '.bw_token' if this file is available.
#' If no token is available, or the refresh argument is TRUE, a new token will be downloaded and cached from the API.
#'
#' @param write_cache
#' (Boolean) Can the token be locally cached.
#' If TRUE, the token will be cached in the current working directory in '.bw_token'.
#' If FALSE, the token will be not be cached locally but will be available for the current R session.
#'
#' @return
#' @export
#'
#' @examples
#' bwr_auth(un = "mickey@mouse.com", pw = "itsasmallworld")
bwr_auth <- function(un, pw, refresh = FALSE, cache = TRUE) {
  if ( refresh | !file.exists(".bw_token") ) {
    token_url <- paste0("https://newapi.brandwatch.com/oauth/token?",
                          "username=", un,
                          "&grant_type=api-password&client_id=brandwatch-api-client",
                          "&password=", URLencode(pw))
    r <- httr::POST(token_url)
    httr::stop_for_status(r)
    token <- httr::content(r)$access_token
    if (cache) saveRDS(token, ".bw_token")
    } else {
      token <- readRDS(".bw_token")
      message("Token read from cached file '.bw_token'")
  }
  Sys.setenv("BW_TOKEN" = token)
}