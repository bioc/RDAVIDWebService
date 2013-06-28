\docType{data}
\name{funChart1}
\alias{funChart1}
\alias{funChart2}
\title{DAVID's website Functional Annotation Chart example files}
\format{funChart1/2 are data.frame for demoList1/2 input ids,
respectively, with the following columns.
\describe{
\item{Category}{factor with the main categories under used in the present
analysis.}
\item{Term}{character with the name of the term in format id~name (if
available).}
\item{Count}{integer with the number of ids of the gene list that belong to
this term.}
\item{X.}{after converting user input gene IDs to  corresponding DAVID gene
ID, it refers to the percentage of DAVID genes in the list assoicated with
particular annotation term. Since DAVID gene ID is unique per gene, it is
more accurate to use DAVID ID percentage to present the gene-annotation
association by removing any redundency in user gene list, i.e. two user IDs
represent same gene.}
\item{PValue}{numeric with the EASE Score of the term (see DAVID Help
page).}
\item{Genes}{character in comma separated style with the genes present in
the term.}
\item{List.Total, Pop.Hits, Pop.Total}{integers (in addition to Count) to
build the 2x2 contingency table in order to compute the EASE Score (see
DAVID Help page).}
\item{Fold.Enrichment}{numeric with the ratio of the two proportions. For
example, if 40/400 (i.e. 10\%) of your input genes involved in "kinase
activity" and the background information is 300/30000 genes (i.e. 1\%)
associating with "kinase activity", roughly 10\%/1\%=10 fold enrichment.}
\item{Bonferroni, Benjamini, FDR}{numerics with p-value adjust different
criterias (see p.adjust)}
}}
\usage{
  data(funChart1)

  data(funChart2)
}
\description{
  These datasets correspond to the reports obtained using
  Functional Annotation Chart Reports in the Database for
  Annotation, Visualization and Integrated Discovery
  (DAVID) website, using as input file the ones provided
  for demo purposes (demoList1 or demoList2) with
  GOTERM_BP_ALL, GOTERM_MF_ALL and GOTERM_CC_ALL
  categories.
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
\keyword{datasets}

