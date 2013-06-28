\docType{class}
\name{DAVIDGeneCluster-class}
\alias{DAVIDGeneCluster-class}
\title{class "DAVIDGeneCluster}
\description{
  This class represents the output of a DAVID Gene
  Functional Classification Tool report.
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
  \item{\code{initialize}}{\code{signature(.Object="DAVIDGeneCluster",
  fileName="character")}: basic cluster report file
  parser.}
  \item{\code{DAVIDGeneCluster}}{\code{signature(fileName="character")}:
  high level gene cluster report file parser.}
  \item{\code{ids}}{\code{signature(object="DAVIDGeneCluster")}:
  list with the member ids within each cluster.}
  \item{\code{genes}}{\code{signature(object="DAVIDGeneCluster")}:
  list with the DAVIDGenes members within each cluster.}
  \item{\code{plot2D}}{\code{signature(object="DAVIDGeneCluster",
  color=c("FALSE"="black","TRUE"="green"), names=FALSE)}:
  ggplot2 tile plot with gene membership to each cluster.}
  }
}
\examples{
{
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

##Then, we can obtain the ids of the members calling clusterGenes object
##which is a DAVIDGenes class or directly using ids on davidGeneCluster1.
ids(clusterGenes)
ids(davidGeneCluster1)[[higherEnrichment]]

##Obtain the genes of the first cluster using davidGeneCluster1 object.
##Or, using genes on DAVIDGenes class once we get the members of the cluster.
genes(davidGeneCluster1)[[1]]
genes(members(davidGeneCluster1)[[1]])

##Finally, we can inspect a 2D tile membership plot, to visually inspect for
##overlapping of genes across the clusters. Or use a scaled version of gene
##names to see the association of gene cluster, e.g., cluster 3 is related to
##ATP genes.
plot2D(davidGeneCluster1)
plot2D(davidGeneCluster1,names=TRUE)+
theme(axis.text.y=element_text(size=rel(0.9)))
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
  Other DAVIDGeneCluster:
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
  \code{\link{as}}, \code{\link{as}}, \code{\link{genes}},
  \code{\link{genes}}, \code{\link{genes}},
  \code{\link{genes}}, \code{\link{ids}},
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

