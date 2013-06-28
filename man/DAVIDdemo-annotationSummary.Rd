\docType{data}
\name{annotationSummary1}
\alias{annotationSummary1}
\alias{annotationSummary2}
\title{DAVID's website annotation summary example files}
\format{annotationSummary1/2 are data.frame for demoList1/2 input ids,
respectively, with the following columns.
\describe{
\item{Main.Category}{factor with the main categories used in the
present analysis.}
\item{ID}{integer to identify the annotation category.}
\item{Name}{character with the name of category (the ones available in
getAllAnnotationCategoryNames function).}
\item{X.}{numeric with the percentage of the gene list ids present in the
term.}
\item{Count}{integer with the number of ids of the gene list that belong to
this term.}
}}
\usage{
  data(annotationSummary1)

  data(annotationSummary2)
}
\description{
  These datasets correspond to the unfolded main summary
  categories data obtained in the Annotation Summary
  Results page in the Database for Annotation,
  Visualization and Integrated Discovery (DAVID) website,
  using as input file, the ones provided for demo purposes
  (demoList1 or demoList2) with default options. No
  statistical analysis is performed on these results.
}
\author{
  Cristobal Fresno and Elmer A Fernandez
}
\references{
  \enumerate{ \item The Database for Annotation,
  Visualization and Integrated Discovery
  (\url{davidgeneList.abcc.ncifcrf.gov}) \item Huang, D.
  W.; Sherman, B. T.; Tan, Q.; Kir, J.; Liu, D.; Bryant,
  D.; Guo, Y.; Stephens, R.; Baseler, M. W.; Lane, H. C.;
  Lempicki, R. A. DAVID Bioinformatics Resources: expanded
  annotation database and novel algorithms to better
  extract biology from large gene lists. Nucleic Acids Res,
  Laboratory of Immunopathogenesis and Bioinformatics,
  SAIC-Frederick, Inc., National Cancer Institute at
  Frederick, MD 21702, USA., 2007, 35, W169-W175 \item
  DAVID Help page
  \url{http://david.abcc.ncifcrf.gov/helps/functional_annotation.html#summary}
  }
}
\seealso{
  Other DataExamples: \code{\link{demoList1}},
  \code{\link{demoList2}}, \code{\link{geneList1}},
  \code{\link{geneList2}}
}
\keyword{datasets}

