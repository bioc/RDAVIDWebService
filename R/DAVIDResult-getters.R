#' Getters for \code{DAVIDResult} object
#'
#' Obtain DAVIDResult slot information, according to the given function call 
#' (see values). 
#' 
#' @param object DAVIDResult class object.
#' 
#' @return according to the call one of the following objects can be returned
#'  \item{type}{character with type slot datum.}
#'
#' @author Cristobal Fresno and Elmer A Fernandez
#' 
#' @docType methods
#' @exportMethod type
#' @name type
#' @rdname DAVIDResult-getters
#' @usage \S4method{type}{DAVIDResult}(object)
#' @aliases type,DAVIDResult-method
setGeneric(name="type", def=function(object){standardGeneric("type")})
#'
#' @exportMethod type
#' @name type
#' @rdname DAVIDResult-getters
#' @inheritParams type
#' @aliases type,DAVIDResult-method
#' @family DAVIDResult
setMethod(f="type", signature="DAVIDResult", definition=function(object){
  return(object@type)
})
