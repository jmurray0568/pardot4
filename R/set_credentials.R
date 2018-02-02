#' Set the Pardot user credentials
#'
#' Simple function to set the users pardot credentials in an R environment variable
#'
#' @param pardot_username A string containing your Pardot UserName
#' @param pardot_password A string containing your Pardot Password
#' @param pardot_user_key A string containing your Pardot User Key
#'
#' @examples
#' \dontrun{
#' set_credentials("your-username", "your-password", "your-user-key")}
#' @export
#' set_credentials


set_credentials <- function(pardot_username, pardot_password, pardot_user_key){
  if(!is.null(pardot_username) && !is.null(pardot_password) && !is.null(pardot_user_key)) {
    .paRdotEnv$data$pardot_username <- pardot_username
    .paRdotEnv$data$pardot_password <- pardot_password
    .paRdotEnv$data$pardot_user_key <- pardot_user_key
    Sys.setenv("PARDOT_USER_KEY" = pardot_user_key)
  }
  else{
    warning("Warning -- Pardot credentials must be set!")
  }
  pardot_client.authenticate()
}
