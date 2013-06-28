#' \code{ids} for the different DAVIDWebService package class objects
#'
#' Obtain ids related information, according to the given function call 
#' (see Values). 
#' 
#' @param object DAVIDWebService class object. Possible values are:
#'   DAVIDGenes, DAVIDFunctionalAnnotationChart, DAVIDGeneCluster or
#'   DAVIDTermCluster.
#'
#' @return according to the call one of the following objects can be returned
#'  \item{DAVIDGenes}{character vector with gene submitted ids.}
#'  \item{DAVIDFunctionalAnnotationChart}{list with character/integer vector of
#'  ids of the corresponding "Category".}
#'  \item{DAVIDGeneCluster, DAVIDTermCluster}{list with character/integer vector
#'  of ids of the members of each cluster.}
#'
#' @author Cristobal Fresno and Elmer A Fernandez
#'
#' @docType methods
#' @exportMethod ids
#' @name ids
#' @rdname DAVIDClasses-ids
#' @usage ids(object)
#' @aliases ids
#' 
#' @examples
#' {
#' ##DAVIDGenes example:
#' ##Load Show Gene List file report for the input demo file 1, using data
#' ##function. Then, create a DAVIDGenes object using the loaded data.frame
#' ##geneList1. Once, the report is loaded, we can retrieve the ids.
#' data(geneList1)
#' davidGenes1<-DAVIDGenes(geneList1)
#' ids(davidGenes1)
#'
#' ##DAVIDFunctionalAnnotationChart example:
#' ##Load the Functional Annotation Chart file report for the input demo 
#' ##file 2, using data function. Then, create a DAVIDFunctionalAnnotationChart
#' ##object using the loaded data.frame funChart2. Once the report is loaded,
#' ##the user can obtain the ids of the genes present in each Term, as a list of
#' ##character vector.
#' data(funChart2)
#' davidFunChart2<-DAVIDFunctionalAnnotationChart(funChart2)
#' ids(davidFunChart2)
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
#' ##idea, and the inspect the cluster with higher Enrichment Score, to see
#' ##which members belong to it, etc. Or simply returning the whole cluster as
#' ##a list with EnrichmentScore and Members.
#' summary(davidGeneCluster1)
#' higherEnrichment<-which.max(enrichment(davidGeneCluster1))
#' clusterGenes<-members(davidGeneCluster1)[[higherEnrichment]]
#' wholeCluster<-cluster(davidGeneCluster1)[[higherEnrichment]]
#' 
#' ##Now, we can obtain the ids of the  first cluster directly using
#' ##davidGeneCluster1 or by using DAVIDGenes class on the same cluster. 
#' ids(davidGeneCluster1)[[1]]
#' ids(members(davidGeneCluster1)[[1]])
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
#' ##idea, and the inspect the cluster with higher Enrichment Score, to see
#' ##which members belong to it, etc. Or simply returning the whole cluster as a
#' ##list with EnrichmentScore and Members.
#' summary(davidTermCluster2)
#' higherEnrichment<-which.max(enrichment(davidTermCluster2))
#' clusterGenes<-members(davidTermCluster2)[[higherEnrichment]]
#' wholeCluster<-cluster(davidTermCluster2)[[higherEnrichment]]
#' 
#' ##Then, we can obtain the ids of the term members calling clusterGenes object
#' ##which is a DAVIDFunctionalAnnotationChart class or directly using ids on
#' ##davidTermCluster2 for the higherEnrichment cluster. 
#' ids(clusterGenes)
#' ids(davidTermCluster2)[[higherEnrichment]]
#' }
#'
setGeneric(name="ids", def=function(object){standardGeneric("ids")})
#'
#' @exportMethod ids
#' @name ids
#' @rdname DAVIDClasses-ids
#' @inheritParams ids
#' @usage \S4method{ids}{DAVIDGenes}(object)
#' @aliases ids,DAVIDGenes-method
#' @family DAVIDGenes 
setMethod(f="ids", signature=signature("DAVIDGenes"), 
  definition=function(object){
    ##Check if ID exits
    ans<-character(0)
    if(nrow(object)>0){
      ans<-object$ID
    }
    return(ans)
  }
)
#'
#' @name ids
#' @rdname DAVIDClasses-ids
#' @inheritParams ids
#' @usage \S4method{ids}{DAVIDFunctionalAnnotationChart}(object)
#' @aliases ids,DAVIDFunctionalAnnotationChart-method
#' @family DAVIDFunctionalAnnotationChart
setMethod(f="ids", signature=signature("DAVIDFunctionalAnnotationChart"),
  definition=function(object){
    ##Default value
    ans<-list()
    if(nrow(object)){
      ans<-strsplit(gsub(pattern=" ",replacement="", object$Genes),split=",")
    }
    return(ans)
})
#'
#' @name ids
#' @rdname DAVIDClasses-ids
#' @inheritParams ids
#' @usage \S4method{ids}{DAVIDGeneCluster}(object)
#' @aliases ids,DAVIDGeneCluster-method
#' @family DAVIDGeneCluster
setMethod(f="ids",signature=signature("DAVIDGeneCluster"), 
  definition=function(object){
    return(lapply(object@cluster, function(clust){ids(clust$Members)}))
})
#'
#' @name ids
#' @rdname DAVIDClasses-ids
#' @inheritParams ids
#' @usage \S4method{ids}{DAVIDTermCluster}(object)
#' @aliases ids,DAVIDTermCluster-method
#' @family DAVIDTermCluster
setMethod(f="ids", signature=signature("DAVIDTermCluster"),
  definition=function(object){
    lapply(members(object), ids)
})
