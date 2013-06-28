#' Basic console output
#'
#' The different implementations of show function for the DAVIDWebService
#' package classes.
#' 
#' @param object DAVIDXX class members (where XX stands for Result, Genes, 
#'  Term/GeneCluster, FunctionalAnnotationChart/Table or DAVIDWebService).
#'
#' @return Basic console output.
#'   
#' @author Cristobal Fresno and Elmer A Fernandez
#'
#' @docType methods
#' @exportMethod show
#' @name show
#' @rdname DAVIDClasses-show
#' @usage \S4method{show}{DAVIDResult}(object)
#' @aliases show,DAVIDResult-method
#' @family DAVIDResult DAVIDGenes DAVIDFunctionalAnnotationChart DAVIDCluster
#' @examples
#' {
#' ##DAVIDGenes example:
#' ##Load Show Gene List file report for the input demo file 1, using data
#' ##function. Then, create a DAVIDGenes object using only the head of the
#' ##loaded data.frame geneList1 (just to keep it simple). 
#' data(geneList1)
#' davidGenes1<-DAVIDGenes(head(geneList1))
#' davidGenes1
#' 
#' ##DAVIDFunctionalAnnotationChart example
#' ##Load the Functional Annotation Chart file report for the input demo 
#' ##file 2, using data function. Then, create a DAVIDFunctionalAnnotationChart
#' ##object using the head of the loaded data.frame funChart2 (just to keep
#' ##it simple). 
#' data(funChart2)
#' davidFunChart2<-DAVIDFunctionalAnnotationChart(head(funChart2))
#' davidFunChart2
#'
#' ##DAVIDFunctionalAnnotationTable example:
#' ##Load the Functional Annotation Table file report for the input demo 
#' ##file 1, using data function. Then, create a DAVIDFunctionalAnnotationTable
#' ##object using the loaded data.frame annotationTable1. 
#' data(annotationTable1)
#' davidFunTable1<-DAVIDFunctionalAnnotationTable(annotationTable1)
#' davidFunTable1
#' }
setMethod(f="show", signature="DAVIDResult", definition=function(object){
  cat("DAVID Result object\n")
  cat("Result type: ", object@type, "\n")
})
#'
#' @exportMethod show
#' @name show
#' @rdname DAVIDClasses-show
#' @inheritParams show
#' @usage \S4method{show}{DAVIDGenes}(object)
#' @aliases show,DAVIDGenes-method
setMethod(f="show", signature=signature("DAVIDGenes"), 
  definition=function(object){
    callNextMethod(object)
    if(nrow(object)>0){
      methods::show(object[1:nrow(object), ])
    }else{
      show(data.frame())
    }
})
#'
#' @exportMethod show
#' @name show
#' @rdname DAVIDClasses-show
#' @inheritParams show
#' @usage \S4method{show}{DAVIDFunctionalAnnotationChart}(object)
#' @aliases show,DAVIDFunctionalAnnotationChart-method
setMethod(f="show", signature=signature("DAVIDFunctionalAnnotationChart"),
  definition=function(object){
    callNextMethod(object)
    if(nrow(object)>0){
      methods::show(object[1:nrow(object), ])
    }else{
      show(data.frame())
    }
})
#'
#' @exportMethod show
#' @name show
#' @rdname DAVIDClasses-show
#' @inheritParams show
#' @usage \S4method{show}{DAVIDCluster}(object)
#' @aliases show,DAVIDCluster-method
setMethod(f="show", signature="DAVIDCluster", definition=function(object){
  ##The header
  callNextMethod()
  cat("Number of cluster: ", ifelse(length(object@cluster)==0,
    "Empty object", length(object@cluster)), "\n")  
})
#'
#' @exportMethod show
#' @name show
#' @rdname DAVIDClasses-show
#' @inheritParams show
#' @usage \S4method{show}{DAVIDFunctionalAnnotationTable}(object)
#' @aliases show,DAVIDFunctionalAnnotationTable-method
setMethod(f="show", signature=signature("DAVIDFunctionalAnnotationTable"),
  definition=function(object){
    callNextMethod(object)
    cat("Genes: ")
    if(nrow(object@Genes)==0){
      cat("empty object \n")
      cat("Available categories: empty object \n")
    }else{
      cat(nrow(object@Genes),
        " with (unique:", nrow(uniqueIds(object@Genes)),
        ", duplicate:", nrow(object@Genes)-nrow(uniqueIds(object@Genes)), ")\n")
      cat("Available categories: ", toString(names(object@Dictionary)), "\n")
    }
    return(invisible())
})
#'
#' @exportMethod show
#' @name show
#' @rdname DAVIDClasses-show
#' @inheritParams show
#' @usage \S4method{show}{DAVIDWebService}(object)
#' @aliases show,DAVIDWebService-method
setMethod(f="show", signature=signature("DAVIDWebService"),
  definition=function(object){
    object$show()
})
