\docType{data}
\name{geneList1}
\alias{geneList1}
\alias{geneList2}
\title{DAVID's website gene list example files}
\format{geneList1/2 are data.frame for demoList1/2 input ids,
respectively, with the following columns.
\describe{
\item{ID}{character with the Gene List ID present in DAVID knowledge base, in
the submitted type. If more than one ids map to the same DAVID ID, the
record is a comma separated character.}
\item{Name}{character with the name of the gene as seen in DAVID knowledge
base, in a comma separated fashion (if more than one ID maps to the same
DAVID ID).}
\item{Species}{factor with the name of the Specie.}
}}
\usage{
  data(geneList1)

  data(geneList2)
}
\description{
  These datasets correspond to the reports obtained using
  Show Gene List in the Database for Annotation,
  Visualization and Integrated Discovery (DAVID) website,
  using as input file the ones provided for demo purposes
  (demoList1 or demoList2) with default options.
}
\author{
  Cristobal Fresno and Elmer A Fernandez
}
\references{
  \enumerate{ \item The Database for Annotation,
  Visualization and Integrated Discovery
  (\url{david.abcc.ncifcrf.gov}) \item Huang, D. W.;
  Sherman, B. T.; Tan, Q.; Kir, J.; Liu, D.; Bryant, D.;
  Guo, Y.; Stephens, R.; Baseler, M. W.; Lane, H. C.;
  Lempicki, R. A. DAVID Bioinformatics Resources: expanded
  annotation database and novel algorithms to better
  extract biology from large gene lists. Nucleic Acids Res,
  Laboratory of Immunopathogenesis and Bioinformatics,
  SAIC-Frederick, Inc., National Cancer Institute at
  Frederick, MD 21702, USA., 2007, 35, W169-W175 \item
  DAVID Help page
  \url{http://david.abcc.ncifcrf.gov/helps/functional_annotation.html#E3}
  }
}
\seealso{
  Other DataExamples: \code{\link{annotationSummary1}},
  \code{\link{annotationSummary2}},
  \code{\link{demoList1}}, \code{\link{demoList2}}
}
\keyword{datasets}

