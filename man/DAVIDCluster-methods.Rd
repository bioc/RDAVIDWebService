\docType{methods}
\name{cluster}
\alias{cluster,DAVIDCluster-method}
\alias{cluster-methods}
\alias{enrichment,DAVIDCluster-method}
\alias{members,DAVIDCluster-method}
\alias{cluster}
\alias{enrichment}
\alias{members}
\title{Methods for \code{DAVIDCluster} class object}
\usage{
  cluster(object)

  \S4method{cluster}{DAVIDCluster}(object)

  enrichment(object)

  \S4method{enrichment}{DAVIDCluster}(object)

  members(object)

  \S4method{members}{DAVIDCluster}(object)
}
\arguments{
  \item{object}{DAVIDCluster class object.}
}
\value{
  according to the call, one of the following objects can
  be returned: \item{cluster}{list with DAVIDCluster object
  slot.} \item{enrichment}{numeric vector with DAVID
  cluster's enrichment score.} \item{members}{list with
  DAVID Cluster's members.}
}
\description{
  Obtain DAVIDCluster related information, according to the
  given function call (see Values).
}
\examples{
{
##DAVIDGeneCluster example:
##Load the Gene Functional Classification Tool file report for the
##input demo list 1 file to create a DAVIDGeneCluster object.
setwd(tempdir())
fileName<-system.file("files/geneClusterReport1.tab.tar.gz",
package="RDAVIDWebService")
untar(fileName)
davidGeneCluster1<-DAVIDGeneCluster(untar(fileName, list=TRUE))
davidGeneCluster1

##Now we can invoke DAVIDCluster ancestor functions to inspect the report
##data of each cluster. For example, we can call summary to get a general
##idea, and then inspect the cluster with the higher Enrichment Score, to see
##which members belong to it, etc. or simply, returning the whole cluster as
##a list with EnrichmentScore and Members.
summary(davidGeneCluster1)
higherEnrichment<-which.max(enrichment(davidGeneCluster1))
clusterGenes<-members(davidGeneCluster1)[[higherEnrichment]]
wholeCluster<-cluster(davidGeneCluster1)[[higherEnrichment]]


##DAVIDTermCluster example:
##Load the Gene Functional Classification Tool file report for the
##input demo file 2 to create a DAVIDGeneCluster object.
setwd(tempdir())
fileName<-system.file("files/termClusterReport2.tab.tar.gz",
package="RDAVIDWebService")
untar(fileName)
davidTermCluster2<-DAVIDTermCluster(untar(fileName, list=TRUE))
davidTermCluster2

##Now we can invoke DAVIDCluster ancestor functions to inspect the report
##data of each cluster. For example, we can call summary to get a general
##idea, and then inspect the cluster with the higher Enrichment Score, to see
##which members belong to it, etc. Or simply returning the whole cluster as a
##list with EnrichmentScore and Members.
summary(davidTermCluster2)
higherEnrichment<-which.max(enrichment(davidTermCluster2))
clusterGenes<-members(davidTermCluster2)[[higherEnrichment]]
wholeCluster<-cluster(davidTermCluster2)[[higherEnrichment]]
}
}
\author{
  Cristobal Fresno and Elmer A Fernandez
}
\seealso{
  Other DAVIDCluster: \code{\link{DAVIDCluster-class}},
  \code{\link{dictionary}}, \code{\link{dictionary}},
  \code{\link{membership}}, \code{\link{membership}},
  \code{\link{subset}}, \code{\link{subset}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}

  Other DAVIDCluster: \code{\link{DAVIDCluster-class}},
  \code{\link{dictionary}}, \code{\link{dictionary}},
  \code{\link{membership}}, \code{\link{membership}},
  \code{\link{subset}}, \code{\link{subset}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}

  Other DAVIDCluster: \code{\link{DAVIDCluster-class}},
  \code{\link{dictionary}}, \code{\link{dictionary}},
  \code{\link{membership}}, \code{\link{membership}},
  \code{\link{subset}}, \code{\link{subset}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}
}

