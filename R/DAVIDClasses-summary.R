#' Basic summary for DAVIDWebService package classes.
#'
#' The different implementations of summary function for the DAVIDWebService
#' package classes.
#' 
#' @param object DAVIDXX class members (where XX stands for Term/GeneCluster,
#' GODag or DAVIDWebService).
#' @param ... Additional parameters.
#'
#' @return data.frame with summary output.
#'   
#' @author Cristobal Fresno and Elmer A Fernandez
#'
#' @docType methods
#' @exportMethod summary
#' @name summary
#' @rdname DAVIDClasses-summary
#' @usage summary(object, ...)
#' @aliases summary
setGeneric("summary")
#'
#' @exportMethod summary
#' @name summary
#' @rdname DAVIDClasses-summary
#' @usage \S4method{summary}{DAVIDCluster}(object)
#' @aliases summary,DAVIDCluster-method
#' @family DAVIDCluster
#' @examples
#' {
#' ##DAVIDGODag example:
#' ##Load the Functional Annotation Chart file report for the input demo 
#' ##file 2, using data function. Then, create a DAVIDGODag object using
#' ##Molecular Function main category of DAVIDFunctionalAnnotationChart object,
#' ##obtained from the loaded data.frame funChart2. In addition, we have
#' ##selected a threshold pvalue of 0.001 and removed unattached nodes, in case
#' ##DAVID/GO.db database are not using the same version.
#' data(funChart2)
#' davidGODag<-DAVIDGODag(DAVIDFunctionalAnnotationChart(funChart2), type="MF",
#'   pvalueCutoff=0.001, removeUnattached=TRUE)
#' summary(davidGODag)
#'
#'
#' ##DAVIDGeneCluster example:
#' ##Load the Gene Functional Classification Tool file report for the
#' ##input demo list 1 file to create a DAVIDGeneCluster object.
#' setwd(tempdir())
#' fileName<-system.file("files/geneClusterReport1.tab.tar.gz",
#'   package="RDAVIDWebService")
#' untar(fileName)
#' davidGeneCluster1<-DAVIDGeneCluster(untar(fileName, list=TRUE))
#' davidGeneCluster1
#' 
#' ##Now we can invoke DAVIDCluster ancestor functions to inspect the report
#' ##data, of each cluster. For example, we can call summary to get a general
#' ##idea
#' summary(davidGeneCluster1)
#'
#'
#' ##DAVIDTermCluster example:
#' ##Load the Gene Functional Classification Tool file report for the
#' ##input demo file 2 to create a DAVIDGeneCluster object.
#' setwd(tempdir())
#' fileName<-system.file("files/termClusterReport2.tab.tar.gz",
#'   package="RDAVIDWebService")
#' untar(fileName)
#' davidTermCluster2<-DAVIDTermCluster(untar(fileName, list=TRUE))
#' davidTermCluster2
#' 
#' ##Now we can invoke DAVIDCluster ancestor functions to inspect the report
#' ##data, of each cluster. For example, we can call summary to get a general
#' ##idea
#' summary(davidTermCluster2)
#' }
#'
setMethod(f="summary",signature = signature("DAVIDCluster"), definition =
function(object){
  ##Default output
  out<-data.frame(Cluster=integer(0), Enrichment =numeric(0),
    Members=integer(0))

  ##Check for results
  if(length(object@cluster)){
    out<-data.frame(Cluster=1:length(object@cluster), 
      Enrichment=enrichment(object), 
      Members=unlist(lapply(members(object), nrow)))
  }
  return(out)
})
#'
#' @exportMethod summary
#' @name summary
#' @rdname DAVIDClasses-summary
#' @usage \S4method{summary}{DAVIDGODag}(object, ...)
#' @aliases summary,DAVIDGODag-method
#' @family DAVIDGODag
setMethod(f="summary", signature=signature("DAVIDGODag"), 
  definition=function(object, ...){
  ##Use previous summary
  out<-callNextMethod(object, ...)

  ##remove not calculated columns
  out$OddsRatio<-NULL
  out$ExpCount<-NULL
  
  ##add additional DAVID attributes
  out$Percent<-percentages(object)[out[, 1]]
  out$List.Total<-listTotals(object)[out[, 1]]
  out$Pop.Total<-popTotals(object)[out[, 1]]
  out$Fold.Enrichment<-foldEnrichments(object)[out[, 1]]
  out$Bonferroni<-bonferronis(object)[out[, 1]]
  out$Benjamini<-benjaminis(object)[out[, 1]]
  out$FDR<-fdrs(object)[out[, 1]]
  
  return(out)
})
#'
#' @exportMethod summary
#' @name summary
#' @rdname DAVIDClasses-summary
#' @usage \S4method{summary}{DAVIDWebService}(object)
#' @aliases summary,DAVIDWebService-method
#' @family DAVIDWebService
setMethod(f="summary", signature=signature("DAVIDWebService"), 
  definition=function(object){
  object$summary()
})
