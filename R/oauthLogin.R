#' @title getOAuthToken (app_url,app,api,scope)
#' @description retrieve authorization token from DATIM/DHIS2
#' @param app OAuth2 client details - Name, Client ID, Client Secret,
#' redirect URIs
#' @param redirect_uri Redirect_uri listed under client details
#' @param api Endpoint for authorization - base url, authorize endpoint, access
#' token endpoint
#' @param scope the scope of what the client will return. "All" is default for
#' DHIS2
#' @return access token to specified OAuth2 Client
#'
getOAuthToken <- function(redirect_uri, app, api, scope) {

  token <- httr::oauth2.0_token(
    app = app,
    endpoint = api,
    scope = scope,
    use_basic_auth = TRUE,
    oob_value = redirect_uri,
    cache = FALSE
  )

  return(token)
}


###############################################################################
#' @export
#' @title loginToDATIMOAuth()
#' @description login to a datim or dhis2 api using Oauth2.
#' This function creates a d2Session login object in the
#' environment calling the login function.
#' E.g. global environment or R-shiny session. Thus you do not need to assign
#' the output of this function to a variable as it creates the variable/object
#' as a side effect.
#' @param base_url The base url for the instance you are authenticating against.
#' @param token The authorization token granted after using getOAuthToken()
#' @param redirect_uri Redirect_uri listed under client details
#' @param app OAuth2 client details - Name, Client ID, Client Secret,
#' redirect URIs
#' @param api Endpoint for authorization - base url, authorize endpoint,
#' access token endpoint
#' @param scope The scope of what the client will return. "All" is default for
#' DHIS2
#' @param d2_session_name the variable name for the d2Session object. The
#' default name is d2_default_session and will be used by other datimutils
#' functions by default when connecting to datim. Generally a custom name
#' should only be needed if you need to log into two seperate DHIS2 instances
#' at the same time. If you create a d2Session object with a custom name then
#' this object must be passed to other datimutils functions explicitly
#' @param d2_session_envir the environment in which to place the R6 login
#' object, default is the immediate calling environment
loginToDATIMOAuth <- function(
    base_url = NULL,
    token = NULL,
    redirect_uri = NULL,
    app = NULL,
    api = NULL,
    scope = NULL,
    d2_session_name = "d2_default_session",
    d2_session_envir = parent.frame()) {


  if (is.null(token)) {

    token <- getOAuthToken(redirect_uri, app, api, scope)
  } else {
    token <- token #For Shiny
  }

  # form url
  url <- utils::URLencode(URL = paste0(base_url, "api", "/me"))

  handle <- httr::handle(base_url)
  #Get Request
  print("Requesting from server with httr::GET")

  print(paste0("url is ",url))
    r <- httr::GET(
    url,
    config(token = token),
    httr::timeout(60),
    handle = handle
  )

  print("Printing status code from request")

  if (r$status_code != 200L) {
    stop("Could not authenticate you with the server!")
  } else {
    me <- jsonlite::fromJSON(httr::content(r, as = "text"))
    print(me)
    # create the session object in the calling environment of the login function
    assign(d2_session_name,
           d2Session$new(base_url = base_url,
                         handle = handle,
                         me = me),
           envir = d2_session_envir)
  }


}


oauth2_api <- httr::oauth_endpoint(base_url = "https://cop-test.datim.org/uaa/oauth",
                      request=NULL,#Documentation says to leave this NULL for OAuth2
                      authorize = "authorize",
                      access="token"


)

#This is rather open and can be toned back as deemed necessary

oauth2_scope <- "ALL"

# Shiny -------------------------------------------------------------------

has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth codes are present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  return(!is.null(params$code))
}



