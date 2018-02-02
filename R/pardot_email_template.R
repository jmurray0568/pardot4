#' Retrieve Pardot Email Template
#'
#' Make a call to the Pardot API and return the data for the specified email template.
#'
#' @param email_template_id The Pardot ID of the target email template.
#' @param verbose Verbose output. See pardot_client(). 
#' @return A data frame.
#' @examples
#' \dontrun{
#' set_credentials("your-username", "your-password", "your-user-key")
#' df <- pardot_email_template(email_template_id = 21918)}
#' @export pardot_email_template
#'

pardot_email_template <- function(email_template_id, verbose = 0) {
	pardot_client("emailTemplate", "read", identifier_field = "id", identifier = email_template_id, verbose = verbose)
}
