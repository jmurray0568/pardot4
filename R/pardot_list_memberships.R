#' Retrieve Pardot List Memberships
#'
#' Make a call to the Pardot API and return the list memberships matching the specified criteria parameters. 
#'
#' @param ... Comma separated list of parameter name and parameter value pairs. Parameter names are not quoted. 
#'   Allowed parameter names are created_after, created_before, deleted, id_greater_than, 
#'   id_less_than, list_id, updated_before, updated_after.
#' @param verbose Verbose output. See pardot_client(). 
#' @return A data frame. See http://developer.pardot.com/kb/object-field-references/#list-membership.
#' @examples
#' \dontrun{
#' set_credentials("your-username", "your-password", "your-user-key")
#' df <- pardot_list_memberships()
#' df <- pardot_list_memberships(created_after = 'today')}
#' @export pardot_list_memberships
#' @import pryr

pardot_list_memberships <- function(..., verbose = 0) {
    # Evaluate parameters in the context of the parent environment,
    # combine parameters to a querystring e.g. param1=value1&param2=value2&...
    dots <- lapply(pryr::named_dots(...), function(p) {
        eval(p, parent.frame())
    })
    request_pars <- paste(paste(names(dots), unlist(dots), sep = "="), collapse = "&")
    pardot_client("listMembership", "query", request_pars = request_pars, verbose = verbose)  
}
