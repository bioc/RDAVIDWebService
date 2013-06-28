#' \code{categories} for the different DAVIDWebService package class objects
#'
#' Obtain ids related information, according to the given function call 
#' (see Values). 
#' 
#' @param object DAVIDWebService class object. Possible values  are:
#' DAVIDFunctionalAnnotationChart or DAVIDFunctionalAnnotationTable.
#'
#' @return according to the call, one of the following objects can be returned:
#'  \item{DAVIDFunctionalAnnotationChart}{factor vector of the "Category"
#'  column.}
#'  \item{DAVIDFunctionalAnnotationTable}{character vector with the name of
#'  available main categories in the dictionary/membership.}
#'
#' @author Cristobal Fresno and Elmer A Fernandez
#'
#' @docType methods
#' @exportMethod categories
#' @name categories
#' @rdname DAVIDClasses-categories
#' @usage categories(object)
#' @aliases categories,DAVIDFunctionalAnnotationChart-method
#' @examples
#' {
#' ##DAVIDFunctionalAnnotationChart example
#' ##Load the Functional Annotation Chart file report for the input demo 
#' ##file 2, using data function. Then, create a DAVIDFunctionalAnnotationChart
#' ##object using the loaded data.frame funChart2. 
#' data(funChart2)
#' davidFunChart2<-DAVIDFunctionalAnnotationChart(funChart2)
#' 
#' ##In Addition to the usual data.frame accessors, the user can inspect the
#' ##main categories used in the analysis. 
#' categories(davidFunChart2)
#'
#' ##DAVIDFunctionalAnnotationTable example
#' ##Load the Functional Annotation Table file report for the input demo 
#' ##file 1, using data function. Then, create a DAVIDFunctionalAnnotationTable
#' ##object using the loaded data.frame annotationTable1. 
#' data(annotationTable1)
#' davidFunTable1<-DAVIDFunctionalAnnotationTable(annotationTable1)
#'  
#' ##Now, the user can inspect the main categories used in the analysis. 
#' categories(davidFunTable1)
#' }
#'
setGeneric(name="categories", def=function(object){
  standardGeneric("categories")
})
#'
#' @exportMethod categories
#' @name categories
#' @rdname DAVIDClasses-categories
#' @inheritParams categories
#' @usage \S4method{categories}{DAVIDFunctionalAnnotationChart}(object)
#' @aliases categories,DAVIDFunctionalAnnotationChart-method
#' @family DAVIDFunctionalAnnotationChart
setMethod(f="categories", signature=signature("DAVIDFunctionalAnnotationChart"),
  definition=function(object){
    return(levels(object$Category))
})
#'
#' @exportMethod categories
#' @name categories
#' @rdname DAVIDClasses-categories
#' @inheritParams categories
#' @usage \S4method{categories}{DAVIDFunctionalAnnotationTable}(object)
#' @aliases categories,DAVIDFunctionalAnnotationTable-method
#' @family DAVIDFunctionalAnnotationTable
setMethod(f="categories", signature=signature("DAVIDFunctionalAnnotationTable"),
  definition=function(object){
    return(names(object@Dictionary))
})
