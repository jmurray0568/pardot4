.paRdotEnv <- new.env()
.paRdotEnv$data <- list()

.onLoad <- function(libname, pkgname){
  if(is.null(.paRdotEnv$data) == FALSE){
    .paRdotEnv$data <- list(
      token <- NULL
    )
  }
}
