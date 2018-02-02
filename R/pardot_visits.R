#' Retrieve Pardot Visits
#'
#' Make a call to the Pardot API and returns the visits matching the specified criteria parameters. 
#'
#' @param ... Comma separated list of parameter name and parameter value pairs. Parameter names are not quoted. 
#'   Allowed parameter names are ids, visitor_ids, prospect_ids.
#' @param verbose Verbose output. See pardot_client(). 
#' @return A data frame. See http://developer.pardot.com/kb/object-field-references/#visit and http://developer.pardot.com/kb/object-field-references/#visitor-page-view. Note: The field visitor_page_view in the returned data frame contains nested lists. Use pardot_client() parameter unlist_dataframe = TRUE to expand the list field into data frame rows, or unlist_dataframe = FALSE to retain the lists as they are.
#' @examples
#' \dontrun{
#' set_credentials("your-username", "your-password", "your-user-key")
#' df <- pardot_visits()
#' df <- pardot_visits(created_after = 'yesterday', prospect_id = 123) }
#' @export pardot_visits
#' @import pryr

pardot_visits <- function(..., verbose = 0) {
    # Evaluate parameters in the context of the parent environment,
    # combine parameters to a querystring e.g. param1=value1&param2=value2&...
    dots <- lapply(pryr::named_dots(...), function(p) {
        eval(p, parent.frame())
    })
    request_pars <- paste(paste(names(dots), unlist(dots), sep = "="), collapse = "&")
    pardot_client("visit", "query", request_pars = request_pars, verbose = verbose)  
}
