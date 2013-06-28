\docType{class}
\name{DAVIDTermCluster-class}
\alias{DAVIDTermCluster-class}
\title{class "DAVIDTermCluster}
\description{
  This class represents the output of a DAVID Functional
  Annotation Clustering report.
}
\section{Type}{
  This class is a "\code{Concrete}" one.
}

\section{Extends}{
  \itemize{ \item \emph{DAVIDCluster} and uses its
  constructor to parse the report. }
}

\section{Slots}{
  the ones inherited from DAVIDCluster.
}

\section{Methods}{
  \describe{
  \item{\code{initialize}}{\code{signature(.Object="DAVIDTermCluster",
  fileName="character")}: basic cluster report file
  parser.}
  \item{\code{DAVIDTermCluster}}{\code{signature(fileName="character")}:
  high level gene cluster report file parser.}
  \item{\code{ids}}{\code{signature(object="DAVIDTermCluster")}:
  list with the member ids within each cluster.}
  \item{\code{plot2D}}{\code{signature(object="DAVIDTermCluster",
  number=1, color=c("FALSE"="black","TRUE"="green"))}:
  ggplot2 tile plot of genes vs functional annotation
  category membership of the given cluster number.} }
}
\examples{
{
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

##Finally, we can inspect a 2D tile membership plot, to visual inspect for
##overlapping of genes across the term members of the selected cluster.
plot2D(davidTermCluster2, number=higherEnrichment)
}
}
\author{
  Cristobal Fresno and Elmer A Fernandez
}
\references{
  \enumerate{ \item The Database for Annotation,
  Visualization and Integrated Discovery
  (david.abcc.ncifcrf.gov) \item Huang, D. W.; Sherman, B.
  T.; Tan, Q.; Kir, J.; Liu, D.; Bryant, D.; Guo, Y.;
  Stephens, R.; Baseler, M. W.; Lane, H. C. & Lempicki, R.
  A. DAVID Bioinformatics Resources: expanded annotation
  database and novel algorithms to better extract biology
  from large gene lists. Nucleic Acids Res, Laboratory of
  Immunopathogenesis and Bioinformatics, SAIC-Frederick,
  Inc., National Cancer Institute at Frederick, MD 21702,
  USA., 2007, 35, W169-W175 }
}
\seealso{
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
}
\keyword{classes}

