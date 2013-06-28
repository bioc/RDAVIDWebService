#' Accessor methods for \code{DAVIDWebService} class
#'
#' Setter/getters for \code{DAVIDWebService} class fields. 
#'
#' Note that \code{DAVIDWebService} is a Reference class, hence invoke it
#' using object_name$setter/getter(parameters). In  addition,
#' the user can use the S4 version style function call.
#'
#' @param object DAVIDWebService class object.
#' @param mail character with a registered e-mail account at DAVID's website.
#'
#' @return according to the call one of the following objects can be returned
#'  \item{setEmail}{character with the given e-mail to set.}
#'  \item{getEmail}{character with the e-mail under use.}
#'  \item{getstub}{jobjRef object with the stub java object to interface with
#'  DAVID API.}
#'
#' @references 
#' \enumerate{
#'  \item DAVID web \url{http://david.abcc.ncifcrf.gov}
#'  \item DAVID API \url{http://david.abcc.ncifcrf.gov/content.jsp?file=WS.html}
#' }
#' @docType methods
#' @exportMethod setEmail
#' @name setEmail
#' @rdname DAVIDWebService-accessors
#' @usage setEmail(object, mail)
#' @aliases setEmail
#' @family DAVIDWebService
#' @examples
#' {
#' ##Create a DAVIDWebService object
#' david<-DAVIDWebService$new()
#' 
#' ##Invoke Reference class style function setter/getters
#' david$setEmail("valid_mail@@david.org")
#' david$getEmail()
#' stub<-david$getStub()
#'
#' ##Or the equivalent S4 style function call setter/getters
#' setEmail(david, "valid_mail@@david.org")
#' getEmail(david)
#' stub<-getStub(david)
#' }
#'
setGeneric(name="setEmail", def=function(object, mail){
  standardGeneric("setEmail")
})
#'
#' @rdname DAVIDWebService-accessors
#' @inheritParams setEmail
#' @usage \S4method{setEmail}{DAVIDWebService}(object, mail)
#' @aliases setEmail,DAVIDWebService-method
setMethod(f="setEmail",signature=signature("DAVIDWebService"), 
  definition=function(object, mail){
    stopifnot(!missing(mail))
    object$setEmail(mail)
})
#' @name setEmail,DAVIDWebService-method
#' @rdname DAVIDWebService-accessors
#' @usage \S4method{setEmail}{character}(mail)
#' @aliases setEmail,character-method
NULL
#' @exportMethod getEmail
#' @name getEmail
#' @rdname DAVIDWebService-accessors
#' @inheritParams setEmail
#' @usage getEmail(object)
#' @aliases getEmail
setGeneric(name="getEmail", def=function(object){standardGeneric("getEmail")})
#'
#' @rdname DAVIDWebService-accessors
#' @inheritParams setEmail
#' @usage \S4method{getEmail}{DAVIDWebService}(object)
#' @aliases getEmail,DAVIDWebService-method
setMethod(f="getEmail",signature=signature("DAVIDWebService"), 
  definition=function(object){
    object$getEmail()
})
#' @exportMethod getStub
#' @name getStub
#' @rdname DAVIDWebService-accessors
#' @inheritParams setEmail
#' @usage getStub(object)
#' @aliases getStub
setGeneric(name="getStub", def=function(object){standardGeneric("getStub")})
#'
#' @rdname DAVIDWebService-accessors
#' @inheritParams setEmail
#' @usage \S4method{getStub}{DAVIDWebService}(object)
#' @aliases getStub,DAVIDWebService-method
setMethod(f="getStub",signature=signature("DAVIDWebService"), 
  definition=function(object){
    object$getStub()
})
