#' Retrieve Pardot Prospects
#'
#' Make a call to the Pardot API and return the prospects matching the specified criteria parameters. 
#'
#' @param ... Comma separated list of parameter name and parameter value pairs. Parameter names are not quoted. 
#'   Allowed parameter names are assigned, assigned_to_user, created_after, created_before, deleted, grade_equal_to,
#'   grade_greater_than, grade_less_than, id_greater_than, id_less_than, is_starred, last_activity_before, 
#'   last_activity_after, last_activity_never, list_id, new, score_equal_to, score_greater_than, score_less_than, 
#'   updated_after, updated_before.
#' @param verbose Verbose output. See pardot_client(). 
#' @return A data frame. See http://developer.pardot.com/kb/object-field-references/#prospect.
#' @examples
#' \dontrun{
#' set_credentials("your-username", "your-password", "your-user-key")
#' df <- pardot_prospects()
#' df <- pardot_prospects(created_after = 'today')}
#' @export pardot_prospects
#' @import pryr

pardot_prospects <- function(..., verbose = 0) {
	# Evaluate parameters in the context of the parent environment,
	# combine parameters to a querystring e.g. param1=value1&param2=value2&...
    dots <- lapply(pryr::named_dots(...), function(p) {
        eval(p, parent.frame())
    })
    request_pars <- paste(paste(names(dots), unlist(dots), sep = "="), collapse = "&")
    pardot_client("prospect", "query", request_pars = request_pars, verbose = verbose)  
}
