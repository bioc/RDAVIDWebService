\docType{class}
\name{DAVIDCluster-class}
\alias{DAVIDCluster-class}
\title{class "DAVIDCluster}
\description{
  This virtual class represents the output of a DAVID
  "Cluster" report, with "DAVIDTermCluster" and
  "DAVIDGeneCluster" as possible heirs, according to the
  report used.
}
\section{Type}{
  This class is a "\code{Virtual}" one.
}

\section{Extends}{
  \itemize{ \item \emph{DAVIDResult} in the conceptual way.
  }
}

\section{Heirs}{
  \itemize{ \item \code{DAVIDTermCluster}: DAVID's
  Functional Annotation Clustering report. \item
  \code{DAVIDGeneCluster}: DAVID's Functional
  Classification Tool report. }
}

\section{Slots}{
  \describe{ \item{\code{cluster}}{named list with the
  different clustered terms/genes: Members, represented as
  DAVIDGenes object; and EnrichmentScore, a numeric with
  DAVID cluster enrichment score.} }
}

\section{Methods}{
  \describe{
  \item{\code{show}}{\code{signature(object="DAVIDCluster")}:
  basic console output.}
  \item{\code{summary}}{\code{signature(object="DAVIDCluster")}:
  basic summary console output.}
  \item{\code{initialize}}{\code{signature(object="DAVIDCluster",
  fileName="character")}: basic cluster report file
  parser.}
  \item{\code{cluster}}{\code{signature(object="DAVIDCluster")}:
  getter for the corresponding slot.}
  \item{\code{enrichment}}{\code{signature(object="DAVIDCluster")}:
  obtain the enrichment score of each cluster.}
  \item{\code{members}}{\code{signature(object="DAVIDCluster")}:
  obtain the corresponding cluster members.} }
}
\author{
  Cristobal Fresno and Elmer A Fernandez
}
\references{
  \enumerate{ \item The Database for Annotation,
  Visualization and Integrated Discovery
  (david.abcc.ncifcrf.gov) \item Huang, D. W.; Sherman, B.
  T.; Tan, Q.; Kir, J.; Liu, D.; Bryant, D.; Guo, Y.;
  Stephens, R.; Baseler, M. W.; Lane, H. C.; Lempicki, R.
  A. DAVID Bioinformatics Resources: expanded annotation
  database and novel algorithms to better extract biology
  from large gene lists. Nucleic Acids Res, Laboratory of
  Immunopathogenesis and Bioinformatics, SAIC-Frederick,
  Inc., National Cancer Institute at Frederick, MD 21702,
  USA., 2007, 35, W169-W175 }
}
\seealso{
  Other DAVIDCluster: \code{\link{cluster}},
  \code{\link{cluster}}, \code{\link{dictionary}},
  \code{\link{dictionary}}, \code{\link{enrichment}},
  \code{\link{enrichment}}, \code{\link{members}},
  \code{\link{members}}, \code{\link{membership}},
  \code{\link{membership}}, \code{\link{subset}},
  \code{\link{subset}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}
}
\keyword{classes}

