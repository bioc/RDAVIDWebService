#' \code{genes} for the different DAVIDWebService package class objects.
#'
#' Obtain genes related information, according to the given function
#' call (see Values). 
#' 
#' @param object DAVIDGenes or DAVIDGeneCluster class object.
#' @param ids character vector with the ids to fetch.
#' @param ... Additional parameters for internal functions (if applicable).
#' 
#' @return according to the call one of the following objects can be returned
#'  \item{DAVIDGenes}{a DAVIDGenes object with the matched genes of ids
#'  parameter. 
#'  If missing, returns all the genes.}
#'  \item{DAVIDGeneCluster}{list with DAVIDGenes objects for each cluster.}
#'  \item{DAVIDFunctionalAnnotationTable}{a DAVIDGenes objects, according to
#'  ... parameter used internally on genes(DAVIDGenes, ...).}
#'  
#' @author Cristobal Fresno and Elmer A Fernandez
#'
#' @docType methods
#' @exportMethod genes
#' @name genes
#' @rdname DAVIDClasses-genes
#' @usage genes(object, ...)
#' @aliases genes-methods
#' @examples
#' {
#' ##DAVIDGenes example:
#' ##Load Show Gene List file report for the input demo file 1, using data
#' ##function. Then, create a DAVIDGenes object using the loaded data.frame
#' ##geneList1. 
#' data(geneList1)
#' davidGenes1<-DAVIDGenes(geneList1)
#' 
#' ##Now, get the genes using the ids look up parameter with the first
#' ##six ids. If ids omitted, all the available are returned.
#' genes(davidGenes1, ids=head(ids(davidGenes1)))
#'
#' ##DAVIDFunctionalAnnotationTable example:
#' ##Load the Functional Annotation Table file report for the input demo 
#' ##file 1, using data function. Then, create a DAVIDFunctionalAnnotationTable
#' ##object using the loaded data.frame annotationTable1. 
#' data(annotationTable1)
#' davidFunTable1<-DAVIDFunctionalAnnotationTable(annotationTable1)
#'  
#' ##Now we can obtain the genes for the given ids, or the complete list if the
#' ##parameter is omitted.
#' genes(davidFunTable1, id=c("37166_at","41703_r_at"))
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
#'
#' ##Then, we can obtain the genes of the first cluster using davidGeneCluster1
#' ##object. Or, using genes on DAVIDGenes class once we get the members of the
#' ##cluster
#' genes(davidGeneCluster1)[[1]]
#' genes(members(davidGeneCluster1)[[1]])
#' }
#'
setGeneric(name="genes", def=function(object, ...){standardGeneric("genes")})
#'
#' @exportMethod genes
#' @name genes
#' @rdname DAVIDClasses-genes
#' @inheritParams genes
#' @usage \S4method{genes}{DAVIDGenes}(object,ids)
#' @aliases genes,DAVIDGenes-method
#' @family DAVIDGenes
setMethod(f="genes", signature=signature("DAVIDGenes"), 
  definition=function(object, ids){
    ##Sanity check
    ans<-new("DAVIDGenes")
    if(nrow(object)>0){
      ans<-object
      if(!missing(ids)){
        found<-unlist(sapply(ids, function(id){
          which(regexpr(pattern=id,ans$ID)>0)
        }))
        ans<-as(ans[found,], "DAVIDGenes")
        validObject(ans)
      }
    }
    return(ans)
  })
#' @exportMethod genes
#' @name genes
#' @rdname DAVIDClasses-genes
#' @usage \S4method{genes}{DAVIDGeneCluster}(object)
#' @aliases genes,DAVIDGeneCluster-method
#' @family DAVIDGeneCluster
setMethod(f="genes", signature=signature("DAVIDGeneCluster"), 
  definition=function(object){
    return(lapply(object@cluster, function(clust){genes(clust$Members)}))
})
#' @exportMethod genes
#' @name genes
#' @rdname DAVIDClasses-genes
#' @usage \S4method{genes}{DAVIDFunctionalAnnotationTable}(object, ...)
#' @aliases genes,DAVIDFunctionalAnnotationTable-method
#' @family DAVIDFunctionalAnnotationTable
setMethod(f="genes", signature=signature("DAVIDFunctionalAnnotationTable"),
  definition=function(object, ...){
    return(genes(object@Genes, ...))
})
