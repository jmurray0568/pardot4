#' Retrieve Pardot Account
#'
#' Make a call to the Pardot API and return the account data.
#'
#' @param verbose Verbose output. See pardot_client(). 
#' @return A data frame. See http://developer.pardot.com/kb/object-field-references/#account.
#' @examples
#' \dontrun{
#' set_credentials("your-username", "your-password", "your-user-key")
#' df <- pardot_account()}
#' @export pardot_account
#'

pardot_account <- function(verbose = 0) {
    pardot_client("account", "read", verbose = verbose)  
}
