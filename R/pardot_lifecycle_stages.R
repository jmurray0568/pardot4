#' Retrieve Pardot Lifecycle Stages
#'
#' Make a call to the Pardot API and return the lifecycle stages matching the specified criteria parameters. 
#'
#' @param ... Comma separated list of parameter name and parameter value pairs. Parameter names are not quoted. 
#'   Allowed parameter names are id_greater_than, id_less_than.
#' @param verbose Verbose output. See pardot_client(). 
#' @return A data frame. See http://developer.pardot.com/kb/object-field-references/#lifecycle-stage.
#' @examples
#' \dontrun{
#' set_credentials("your-username", "your-password", "your-user-key")
#' df <- pardot_lifecycle_stages()
#' df <- pardot_lifecycle_stages(id_greater_than = '123')}
#' @export pardot_lifecycle_stages
#' @import pryr

pardot_lifecycle_stages <- function(..., verbose = 0) {
	# Evaluate parameters in the context of the parent environment,
	# combine parameters to a querystring e.g. param1=value1&param2=value2&...
    dots <- lapply(pryr::named_dots(...), function(p) {
        eval(p, parent.frame())
    })
    request_pars <- paste(paste(names(dots), unlist(dots), sep = "="), collapse = "&")
    pardot_client("lifecycleStage", "query", request_pars = request_pars, verbose = verbose)  
}
