\docType{methods}
\name{genes}
\alias{genes,DAVIDFunctionalAnnotationTable-method}
\alias{genes,DAVIDGeneCluster-method}
\alias{genes,DAVIDGenes-method}
\alias{genes-methods}
\alias{genes}
\title{\code{genes} for the different DAVIDWebService package class objects.}
\usage{
  genes(object, ...)

  \S4method{genes}{DAVIDGenes}(object,ids)

  \S4method{genes}{DAVIDGeneCluster}(object)

  \S4method{genes}{DAVIDFunctionalAnnotationTable}(object,
  ...)
}
\arguments{
  \item{object}{DAVIDGenes or DAVIDGeneCluster class
  object.}

  \item{ids}{character vector with the ids to fetch.}

  \item{...}{Additional parameters for internal functions
  (if applicable).}
}
\value{
  according to the call one of the following objects can be
  returned \item{DAVIDGenes}{a DAVIDGenes object with the
  matched genes of ids parameter. If missing, returns all
  the genes.} \item{DAVIDGeneCluster}{list with DAVIDGenes
  objects for each cluster.}
  \item{DAVIDFunctionalAnnotationTable}{a DAVIDGenes
  objects, according to ... parameter used internally on
  genes(DAVIDGenes, ...).}
}
\description{
  Obtain genes related information, according to the given
  function call (see Values).
}
\examples{
{
##DAVIDGenes example:
##Load Show Gene List file report for the input demo file 1, using data
##function. Then, create a DAVIDGenes object using the loaded data.frame
##geneList1.
data(geneList1)
davidGenes1<-DAVIDGenes(geneList1)

##Now, get the genes using the ids look up parameter with the first
##six ids. If ids omitted, all the available are returned.
genes(davidGenes1, ids=head(ids(davidGenes1)))

##DAVIDFunctionalAnnotationTable example:
##Load the Functional Annotation Table file report for the input demo
##file 1, using data function. Then, create a DAVIDFunctionalAnnotationTable
##object using the loaded data.frame annotationTable1.
data(annotationTable1)
davidFunTable1<-DAVIDFunctionalAnnotationTable(annotationTable1)

##Now we can obtain the genes for the given ids, or the complete list if the
##parameter is omitted.
genes(davidFunTable1, id=c("37166_at","41703_r_at"))


##DAVIDGeneCluster example:
##Load the Gene Functional Classification Tool file report for the
##input demo list 1 file to create a DAVIDGeneCluster object.
setwd(tempdir())
fileName<-system.file("files/geneClusterReport1.tab.tar.gz",
package="RDAVIDWebService")
untar(fileName)
davidGeneCluster1<-DAVIDGeneCluster(untar(fileName, list=TRUE))

##Then, we can obtain the genes of the first cluster using davidGeneCluster1
##object. Or, using genes on DAVIDGenes class once we get the members of the
##cluster
genes(davidGeneCluster1)[[1]]
genes(members(davidGeneCluster1)[[1]])
}
}
\author{
  Cristobal Fresno and Elmer A Fernandez
}
\seealso{
  Other DAVIDFunctionalAnnotationTable:
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDFunctionalAnnotationTable-class}},
  \code{\link{DAVIDGODag}}, \code{\link{DAVIDGODag}},
  \code{\link{DAVIDGeneCluster}},
  \code{\link{DAVIDGeneCluster}}, \code{\link{DAVIDGenes}},
  \code{\link{DAVIDGenes}}, \code{\link{DAVIDGenes}},
  \code{\link{DAVIDTermCluster}},
  \code{\link{DAVIDTermCluster}}, \code{\link{as}},
  \code{\link{as}}, \code{\link{as}},
  \code{\link{categories}}, \code{\link{categories}},
  \code{\link{categories}}, \code{\link{dictionary}},
  \code{\link{dictionary}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{membership}}, \code{\link{membership}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{subset}}, \code{\link{subset}}

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
  \code{\link{as}}, \code{\link{as}}, \code{\link{ids}},
  \code{\link{ids}}, \code{\link{ids}}, \code{\link{ids}},
  \code{\link{ids}}, \code{\link{initialize}},
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
  \code{\link{as}}, \code{\link{as}}, \code{\link{ids}},
  \code{\link{ids}}, \code{\link{ids}}, \code{\link{ids}},
  \code{\link{ids}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}}
}

