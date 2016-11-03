#' Sign in to the Force.com (Salesforce.com) using OAuth
#'
#' This function retrives a Bearer access token as a session ID from Salesforce.com.
#'
#' @usage rforcecom.oath(username, password, consumerkey, consumersecret, granttype, loginURL, apiVersion)
#' @param username Your username for login to the Salesforce.com. In many cases, username is your E-mail address.
#' @param password Your password for login to the Salesforce.com.
#' @param consumerkey Your consumerkey (client_id) for OAuth. (ex:) "4MVG9AKuBE3rUYDhKvKpmGqBFGu9GZrsiNLlzB0I7N6mtF6LTOMXxnUbN_Zmu9AQN8K.mZc0V35gEP5eO7DKo"
#' @param consumersecret Your consumersecret (client_secret) for OAuth. (ex:) "1234567890123456789"
#' @param granttype. (ex:) "password"
#' @param loginURL (optional) Login URL. If your environment is sandbox specify (ex:) "https://test.salesforce.com/".
#' @param apiVersion (optional) Version of the REST API and SOAP API that you want to use. (ex:) "35.0" Supported versions from v20.0 and up.
#' @return
#' \item{sessionID}{Session ID.}
#' \item{instanceURL}{Instance URL.}
#' \item{apiVersion}{API Version.}
#' @examples
#' \dontrun{
#' # Sign in to the Force.com with OAuth
#' username <- "yourname@@yourcompany.com"
#' password <- "YourPassword"
#' consumerkey <- "4MVG9AKuBE3rUYDhKvKpmGqBFGu9GZrsiNLlzB0I7N6mtF6LTOMXxnUbN_Zmu9AQN8K.mZc0V35gEP5eO7DKo"
#' consumersecret <- "1234567890123456789"
#' granttype <- "password"
#' session <- rforcecom.oauth(username, password, consumerkey, consumersecret, granttype)
#' }
#' @seealso
#'  \code{\link{rforcecom.query}}
#'  \code{\link{rforcecom.search}}
#'  \code{\link{rforcecom.create}}
#'  \code{\link{rforcecom.delete}}
#'  \code{\link{rforcecom.retrieve}}
#'  \code{\link{rforcecom.update}}
#'  \code{\link{rforcecom.upsert}}
#'  \code{\link{rforcecom.getServerTimestamp}}
#'  \code{\link{rforcecom.getObjectDescription}}
#'  \code{\link{rforcecom.getObjectList}}
#'  \code{\link{rforcecom.queryMore}}
#' @keywords connection
#' @export
rforcecom.oauth <-
  function(username, password, consumerkey, consumersecret, granttype, loginURL="https://login.salesforce.com", apiVersion="35.0"){

    if(as.numeric(apiVersion) < 20) stop("The earliest supported API version is 20.0")

    # Named list of OAuth login parameters
    loginparams <- list(
      'grant_type' = granttype,
      'client_id' = consumerkey,
      'client_secret' = consumersecret,
      'username' = username,
      'password' = password
      )

    # HTTP POST
    URL <- paste0(loginURL, "/services/oauth2/token")
    request <- httr::POST(url = URL, body = loginparams)
    response <- httr::content(request, "parsed")

    # Check for login error
    if (exists('error_description', where = response)) stop(response$error_description)

    # BEGIN DEBUG
    if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
    if(exists("rforcecom.debug") && rforcecom.debug){ message(response) }
    # END DEBUG

    # Retrieve sessionID and instanceURL from response
    sessionID <- response$access_token
    instanceURL <- paste0(response$instance_url, "/")

    # Return sessionID, instanceURL, and apiVersion
    return(c(sessionID=sessionID, instanceURL=instanceURL, apiVersion=apiVersion))
  }
