#' Make a call to the Pardot API and return XML or data frame
#'
#' @param object A string containing a Pardot Object
#' @param operator A string containing a Pardot Operator
#' @param identifier_field A string with an optional identifier field. Can be null
#' @param identifier A string with an optional identifier that can be null if identifier_field is null
#' @param request_pars A string of query parameters. Can be null
#' @param result_format A string specifying the result format used for API calls: "json" (default) or "xml". If json, pardot_client() returns a data frame.
#' @param unlist_dataframe A logical, default TRUE. If it is FALSE all fields having embedded lists are returned as they are. If unlist_dataframe is TRUE a field with embedded list(s) is converted to multiple records and/or fields. The values of the other fields are duplicated across these records. Applies to object "visit".
#' @param verbose Verbose output. Integer value, default zero is non-verbose. 1 displays a progress bar consisting of dots and numbers, for respectively every 200th and 1000th received record. 2 displays the successive call urls and the data structure returned by the first call.
#' @return XML or a data frame.
#' @examples
#' \dontrun{
#' set_credentials("your-username", "your-password", "your-user-key")
#' pardot_client("campaign", "query")
#' pardot_client(object = "campaign", operator = "query", 
#'   request_params = "created_after=yesterday&id_greater_than=492276479")}
#' @export pardot_client
#' @import httr
#' @import xml2
#' @import XML
#' @import jsonlite
#' @import dplyr

pardot_client <- function(object, operator, identifier_field=NULL, identifier=NULL, request_pars=NULL, result_format="json", unlist_dataframe = TRUE, verbose = 0) {
  # object & operator are required fields
  # identifier fields / identifier are optional
  # optional field to implement <- api_request_params,"&format=",api_format
  param_list <- (as.list(match.call()))

  if (!exists('api_key')) {
    pardot_client.authenticate()
  } else if (exists('api_key') && api_key == "Login failed" ) {
    pardot_client.authenticate()
  } else {
    request_url <- pardot_client.build_url(object, operator, identifier_field, identifier, request_pars)
	if (result_format == "json") {
		pardot_client.api_call_json(request_url, unlist_dataframe = unlist_dataframe, verbose = verbose)
	} else {
		pardot_client.api_call(request_url)
	}
  }
}

pardot_client.authenticate <- function() {
  # body params must be set in list. Add .env get that will fetch these items
  auth_body  <- list(email = .paRdotEnv$data$pardot_username,
                     password = .paRdotEnv$data$pardot_password,
                     user_key = .paRdotEnv$data$pardot_user_key)

  # make initial API call to authenticate
  fetch_api_call <- POST("https://pi.pardot.com/api/login/version/4", body= auth_body)

  # returns xml node with <api_key>
  api_key <<- xml_text(content(fetch_api_call))
}

pardot_client.api_call_json <- function(request_url, unlist_dataframe = TRUE, verbose = 0) {
	
	# Retrieve results in chunks
	polished_df <- data.frame()
	ready <- FALSE
	chunk_size <- 200
	# Initialize n_offset with value given in request_url
	n_offset <- sub("^.*[?&]n_offset=([0-9]*).*$", "\\1", request_url)
	n_offset0 <- if (n_offset == request_url) 0 else as.integer(n_offset)
	n_offset <- n_offset0
	while (!ready) {
	    # Progress indicator: number for every k, dot for every chunk
		progress_1k <- (n_offset - n_offset0) / 1000
	    if (verbose > 0) cat(ifelse(progress_1k == round(progress_1k, 0), as.character(progress_1k), "."))
	    if (n_offset == n_offset0) {
		    if (verbose > 1) print(request_url)
			raw_df <- pardot_client.get_data_frame(request_url)
			if (verbose > 1) print(str(raw_df))
		} else {
		    iterative_request_url <- 
		        pardot_client.iterative_request_url(request_url, n_offset = n_offset)
		    if (verbose > 1) print(iterative_request_url)
		    raw_df <- pardot_client.get_data_frame(iterative_request_url)
		}
	    n <- nrow(raw_df)
	    # Unnest nested data frames
        flat_df <- flatten(raw_df, recursive = TRUE)
        # Unlist list fields
        if (unlist_dataframe) {
            unlist_flat_df <- pardot_client.unlist_dataframe(flat_df)
            flat_df <- unlist_flat_df
        }
	    # Append
		if (n > 0) {
			n_offset <- n_offset + n
            polished_df <- rbind_pages(list(polished_df, flat_df))
			if (n < chunk_size) ready <- TRUE
		} else {
			ready <- TRUE
		}
	}
	if (verbose > 1 && unlist_dataframe) {
	    if (nrow(polished_df) - n_offset > 0)
	        message(sprintf("Unlist created %d more rows", nrow(polished_df) - n_offset))
	}
	# Substitute dots by underscores
	colnames(polished_df) <- gsub(".", "_", colnames(polished_df), fixed = TRUE) 
	return(polished_df)
}

pardot_client.api_call <- function(request_url) {
  resp <- GET(request_url)

  if ( resp$status != 200 ) {
    pardot_client.authenticate()
    resp <- GET(request_url, content_type_xml())
  }

  xml_response <- xmlNode(content(resp, "parsed"))
  return(xml_response)
}


pardot_client.get_data_frame <- function(theUrl) {
    # GET the url response in json format and convert to list
    # Replace NULL values by NA so that list can be cast to data frame
    respjson <- GET(theUrl, content_type_json())
    if (respjson$status != 200) {
        warning(sprintf("GET returned %s", as.character(respjson$status)))
        return(data.frame())
    }
    res <- fromJSON(content(respjson, as = "text", encoding = "UTF-8"))
    if (res$`@attributes`$stat == "fail") {
        warning(res$err)
        return(data.frame())
    } else if ((names(res))[2] %in% c("account", "email", "emailTemplate", "stats")) {
        res_data  <- pardot_client.nonnull_list(res[[2]])
        d <- as.data.frame(res_data, stringsAsFactors = FALSE)
    } else if ((names(res))[2] == "result") {
        item <- which(names(res[[2]]) %in% c("campaign", "emailClick", "form", "list", "list_membership", "prospect", "prospectAccount", "tag", "tagObject", "visitor", "visitor_activity", "visit"))
        res_data  <- pardot_client.nonnull_list(res[[2]][[item]])
        d <- as.data.frame(res_data, stringsAsFactors = FALSE)
    } else {
        warning("paRdot API response could not be cast to dataframe")
        return(res_data)
    }
    return(d)
}

pardot_client.nonnull_list <- function(list_with_nulls) {
    list_without_nulls <- lapply(list_with_nulls, function(x) {
        if (class(x) == "list")
            unlist(
                lapply(x, function(e) {
                    if(is.null(e)) NA else e
                })
            )
        else if (is.null(x))
            NA
        else
            x
    })
    return(list_without_nulls)
}

pardot_client.build_url <- function(object, operator, identifier_field=NULL, identifier=NULL, request_pars = NULL) {
    identifier_field <- pardot_client.scrub_opts(identifier_field)
    identifier <- pardot_client.scrub_opts(identifier)
    request_pars <- if (length(request_pars) > 0) sub("^&*", "\\&", request_pars)
    request_url <- paste0("https://pi.pardot.com/api/", object,"/version/4/do/", operator, identifier_field, identifier,"?api_key=", api_key, "&user_key=", Sys.getenv("PARDOT_USER_KEY"), request_pars, "&output=bulk&format=json")
  return(request_url)
}

pardot_client.iterative_request_url <- function(request_url, theDate = NULL, n_offset = NULL) {
	# Keep original parameter theDate for backward compatibility
	# Check n_offset first as offset is the preferred method for navigating.
	# Consequence is that n_offset must be named when the function is called.
	if (!missing(n_offset)) {
		iterative_request_url <- paste0(request_url, "&offset=", n_offset)
	} else if (!missing(theDate)) {
		theDate <- gsub(' ', 'T', theDate)
		iterative_request_url <- paste0(request_url,"&created_after=",theDate,"&sort_by=created_at&sort_order=ascending")
	} else {
		iterative_request_url <- request_url
	}
	return(iterative_request_url)
}

pardot_client.scrub_opts <- function(opt) {
  if( is.null(opt) || opt == '' ) {
    return('')
  } else {
    new_opt <- paste0('/',opt)
    return(new_opt)
  }
}

pardot_client.unlist_dataframe <- function(df) {
    df_colclasses <- sapply(df, class)
    df_colclasses_list <- names(df_colclasses[df_colclasses == "list"])
    if (length(df_colclasses_list) == 0) {
        # Nothing to unlist
        return(df)
    }
    # Cast list fields to data frame, making a wider data frame
    df_unlisted <- df %>% rowwise() %>% do({
        # Convert NULLs in list fields to NA to avoid data.frame() casting error
        dfrow <- .
        listfields <- names(dfrow)[unlist(lapply(names(dfrow), function(f) class(dfrow[[f]]))) == "list"]
        if (length(listfields) > 0) {
            dfrow[[listfields]] <- lapply(dfrow[[listfields]], function(v) {
                v[is.null(v)] <- NA
                v
            })
        }
        # Now cast to data frame
        df_unl <- data.frame(dfrow, stringsAsFactors = FALSE)
    })
    return(df_unlisted)
}
