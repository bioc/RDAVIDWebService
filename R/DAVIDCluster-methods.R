#' Methods for \code{DAVIDCluster} class object
#'
#' Obtain DAVIDCluster related information, according to the given function
#' call (see Values). 
#' 
#' @param object DAVIDCluster class object.
#' 
#' @return according to the call, one of the following objects can be returned:
#'  \item{cluster}{list with DAVIDCluster object slot.}
#'  \item{enrichment}{numeric vector with DAVID cluster's enrichment score.}
#'  \item{members}{list with DAVID Cluster's members.}
#'  
#' @author Cristobal Fresno and Elmer A Fernandez
#'
#' @docType methods
#' @exportMethod cluster
#' @name cluster
#' @rdname DAVIDCluster-methods
#' @usage cluster(object)
#' @aliases cluster-methods
#' @examples
#' {
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
#' ##data of each cluster. For example, we can call summary to get a general
#' ##idea, and then inspect the cluster with the higher Enrichment Score, to see
#' ##which members belong to it, etc. or simply, returning the whole cluster as
#' ##a list with EnrichmentScore and Members.
#' summary(davidGeneCluster1)
#' higherEnrichment<-which.max(enrichment(davidGeneCluster1))
#' clusterGenes<-members(davidGeneCluster1)[[higherEnrichment]]
#' wholeCluster<-cluster(davidGeneCluster1)[[higherEnrichment]]
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
#' ##data of each cluster. For example, we can call summary to get a general
#' ##idea, and then inspect the cluster with the higher Enrichment Score, to see
#' ##which members belong to it, etc. Or simply returning the whole cluster as a
#' ##list with EnrichmentScore and Members.
#' summary(davidTermCluster2)
#' higherEnrichment<-which.max(enrichment(davidTermCluster2))
#' clusterGenes<-members(davidTermCluster2)[[higherEnrichment]]
#' wholeCluster<-cluster(davidTermCluster2)[[higherEnrichment]]
#' }
#'
setGeneric(name="cluster", def=function(object){standardGeneric("cluster")})
#'
#' @exportMethod cluster
#' @name cluster
#' @rdname DAVIDCluster-methods
#' @inheritParams cluster
#' @usage \S4method{cluster}{DAVIDCluster}(object)
#' @aliases cluster,DAVIDCluster-method
#' @family DAVIDCluster
setMethod(f="cluster", signature="DAVIDCluster", definition=function(object){
  return(object@cluster)
})
#'
#' @exportMethod enrichment
#' @name enrichment
#' @rdname DAVIDCluster-methods
#' @inheritParams cluster
#' @usage enrichment(object)
#' @aliases enrichment
setGeneric(name="enrichment", def=function(object){
  standardGeneric("enrichment")
})
#'
#' @exportMethod enrichment
#' @name enrichment
#' @rdname DAVIDCluster-methods
#' @inheritParams cluster
#' @usage \S4method{enrichment}{DAVIDCluster}(object)
#' @aliases enrichment,DAVIDCluster-method
#' @family DAVIDCluster
setMethod(f="enrichment", signature="DAVIDCluster", definition=function(object){
  ##Default output
  out<-numeric(0)
  if(length(object@cluster)){
    out<-do.call(c, lapply(object@cluster, function(index){
      index$EnrichmentScore
    }))
  }
  return(out)
})
#'
#' @exportMethod members
#' @name members
#' @rdname DAVIDCluster-methods
#' @inheritParams cluster
#' @usage members(object)
#' @aliases members
setGeneric(name="members", def=function(object){standardGeneric("members")})
#'
#' @exportMethod members
#' @name members
#' @rdname DAVIDCluster-methods
#' @inheritParams cluster
#' @usage \S4method{members}{DAVIDCluster}(object)
#' @aliases members,DAVIDCluster-method
#' @family DAVIDCluster
setMethod(f="members", signature=signature("DAVIDCluster"), 
  definition=function(object){
    return(lapply(object@cluster, function(x){x$Members}))
})
