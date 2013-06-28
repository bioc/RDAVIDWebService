\docType{methods}
\name{ids}
\alias{ids,DAVIDFunctionalAnnotationChart-method}
\alias{ids,DAVIDGeneCluster-method}
\alias{ids,DAVIDGenes-method}
\alias{ids,DAVIDTermCluster-method}
\alias{ids}
\title{\code{ids} for the different DAVIDWebService package class objects}
\usage{
  ids(object)

  \S4method{ids}{DAVIDGenes}(object)

  \S4method{ids}{DAVIDFunctionalAnnotationChart}(object)

  \S4method{ids}{DAVIDGeneCluster}(object)

  \S4method{ids}{DAVIDTermCluster}(object)
}
\arguments{
  \item{object}{DAVIDWebService class object. Possible
  values are: DAVIDGenes, DAVIDFunctionalAnnotationChart,
  DAVIDGeneCluster or DAVIDTermCluster.}
}
\value{
  according to the call one of the following objects can be
  returned \item{DAVIDGenes}{character vector with gene
  submitted ids.}
  \item{DAVIDFunctionalAnnotationChart}{list with
  character/integer vector of ids of the corresponding
  "Category".} \item{DAVIDGeneCluster,
  DAVIDTermCluster}{list with character/integer vector of
  ids of the members of each cluster.}
}
\description{
  Obtain ids related information, according to the given
  function call (see Values).
}
\examples{
{
##DAVIDGenes example:
##Load Show Gene List file report for the input demo file 1, using data
##function. Then, create a DAVIDGenes object using the loaded data.frame
##geneList1. Once, the report is loaded, we can retrieve the ids.
data(geneList1)
davidGenes1<-DAVIDGenes(geneList1)
ids(davidGenes1)

##DAVIDFunctionalAnnotationChart example:
##Load the Functional Annotation Chart file report for the input demo
##file 2, using data function. Then, create a DAVIDFunctionalAnnotationChart
##object using the loaded data.frame funChart2. Once the report is loaded,
##the user can obtain the ids of the genes present in each Term, as a list of
##character vector.
data(funChart2)
davidFunChart2<-DAVIDFunctionalAnnotationChart(funChart2)
ids(davidFunChart2)


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
##data, of each cluster. For example, we can call summary to get a general
##idea, and the inspect the cluster with higher Enrichment Score, to see
##which members belong to it, etc. Or simply returning the whole cluster as
##a list with EnrichmentScore and Members.
summary(davidGeneCluster1)
higherEnrichment<-which.max(enrichment(davidGeneCluster1))
clusterGenes<-members(davidGeneCluster1)[[higherEnrichment]]
wholeCluster<-cluster(davidGeneCluster1)[[higherEnrichment]]

##Now, we can obtain the ids of the  first cluster directly using
##davidGeneCluster1 or by using DAVIDGenes class on the same cluster.
ids(davidGeneCluster1)[[1]]
ids(members(davidGeneCluster1)[[1]])


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
##data, of each cluster. For example, we can call summary to get a general
##idea, and the inspect the cluster with higher Enrichment Score, to see
##which members belong to it, etc. Or simply returning the whole cluster as a
##list with EnrichmentScore and Members.
summary(davidTermCluster2)
higherEnrichment<-which.max(enrichment(davidTermCluster2))
clusterGenes<-members(davidTermCluster2)[[higherEnrichment]]
wholeCluster<-cluster(davidTermCluster2)[[higherEnrichment]]

##Then, we can obtain the ids of the term members calling clusterGenes object
##which is a DAVIDFunctionalAnnotationChart class or directly using ids on
##davidTermCluster2 for the higherEnrichment cluster.
ids(clusterGenes)
ids(davidTermCluster2)[[higherEnrichment]]
}
}
\author{
  Cristobal Fresno and Elmer A Fernandez
}
\seealso{
  Other DAVIDFunctionalAnnotationChart:
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart-class}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDGODag}}, \code{\link{DAVIDGODag}},
  \code{\link{DAVIDGeneCluster}},
  \code{\link{DAVIDGeneCluster}}, \code{\link{DAVIDGenes}},
  \code{\link{DAVIDGenes}}, \code{\link{DAVIDGenes}},
  \code{\link{DAVIDTermCluster}},
  \code{\link{DAVIDTermCluster}}, \code{\link{as}},
  \code{\link{as}}, \code{\link{as}},
  \code{\link{categories}}, \code{\link{categories}},
  \code{\link{categories}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}}

  Other DAVIDGeneCluster:
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDGODag}}, \code{\link{DAVIDGODag}},
  \code{\link{DAVIDGeneCluster}},
  \code{\link{DAVIDGeneCluster}},
  \code{\link{DAVIDGeneCluster-class}},
  \code{\link{DAVIDGenes}}, \code{\link{DAVIDGenes}},
  \code{\link{DAVIDGenes}}, \code{\link{DAVIDTermCluster}},
  \code{\link{DAVIDTermCluster}}, \code{\link{as}},
  \code{\link{as}}, \code{\link{as}}, \code{\link{genes}},
  \code{\link{genes}}, \code{\link{genes}},
  \code{\link{genes}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}}

  Other DAVIDGenes:
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDGODag}}, \code{\link{DAVIDGODag}},
  \code{\link{DAVIDGeneCluster}},
  \code{\link{DAVIDGeneCluster}}, \code{\link{DAVIDGenes}},
  \code{\link{DAVIDGenes}}, \code{\link{DAVIDGenes}},
  \code{\link{DAVIDGenes-class}},
  \code{\link{DAVIDTermCluster}},
  \code{\link{DAVIDTermCluster}}, \code{\link{as}},
  \code{\link{as}}, \code{\link{as}}, \code{\link{genes}},
  \code{\link{genes}}, \code{\link{genes}},
  \code{\link{genes}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}}

  Other DAVIDTermCluster:
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDGODag}}, \code{\link{DAVIDGODag}},
  \code{\link{DAVIDGeneCluster}},
  \code{\link{DAVIDGeneCluster}}, \code{\link{DAVIDGenes}},
  \code{\link{DAVIDGenes}}, \code{\link{DAVIDGenes}},
  \code{\link{DAVIDTermCluster}},
  \code{\link{DAVIDTermCluster}},
  \code{\link{DAVIDTermCluster-class}}, \code{\link{as}},
  \code{\link{as}}, \code{\link{as}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}
}

