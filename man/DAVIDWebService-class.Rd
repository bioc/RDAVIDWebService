\docType{class}
\name{DAVIDWebService-class}
\alias{DAVIDWebService}
\alias{DAVIDWebService-class}
\title{Main class to connect to DAVID Web Service}
\usage{
  DAVIDWebService(...)
}
\arguments{
  \item{...}{additional parameters. See Methods section.}
}
\description{
  A reference class to manage DAVID's Web Service
  connectivity, to run Set Enrichment Analysis (SEA) or
  Modular Enrichment Analysis (MEA) on a candidate list of
  gene/protein(s) with respect to a background list (the
  genome of the organism by default).
}
\details{
  \code{DAVIDWebService} class is implemented as a
  reference class, to manage a single instance connection
  to DAVIS's server by means of web services using a
  registered e-mail. For user registration, go to
  \url{http://david.abcc.ncifcrf.gov/webservice/register.html}.
  The implementation uses Java Remote Method Implementation
  (RMI) to connect the client and server side of DAVID. The
  main functionalities include: \enumerate{ \item
  Connectivity: upload gene/background list/s, change
  gene/background position, select current specie/s, select
  annotations, etc. from R. \item Reports: Submitted Gene
  List, Annotation Category Summary, Gene/Term Clusters,
  Functional Annotation Chart and Functional Annotation
  Table as native R objects. }
}
\section{Fields}{
  \describe{ \item{\code{stub}:}{Java jobjRef which
  corresponds to a
  sample/session/client/stub/DAVIDWebServiceStub object for
  the client side of DAVID.}
  \item{\code{email}:}{character.} }
}

\section{Methods}{
  \describe{ \item{\code{show()}: }{prints DAVIDWebService
  object.} \item{\code{summary()}: }{return a data.frame
  with a summary of all available annotations in DAVID in
  terms of percentage of gene list ids present in the
  category and numbers of terms where they can be found
  (see getAnnotationSummary)} \item{\code{initialize(...,
  email="", url)}:}{constructor for DAVIDWebService object,
  which includes: Java Virtual Machine initialization (...
  if required), and stub initialization with the provided
  email (if present) and using the url parameter for the
  API website.} \item{\code{setEmail(mail)}: }{Set the
  email field with the given registered user email
  parameter for authentication purposes.}
  \item{\code{getEmail()}: }{Returns the current
  authentication email in use.} \item{\code{getStub}:
  }{Returns jobjRef object with the current stub field in
  use.} \item{\code{is.connected()}: }{Check if connected
  to the DAVID server.} \item{\code{connect()}: }{Try to
  establish a connection with the DAVID server using the
  provided email.} \item{\code{getIdTypes()}: }{Returns all
  acceptable DAVID idTypes.}
  \item{\code{getAllAnnotationCategoryNames()}: }{Returns
  all available annotation category names.}
  \item{\code{getDefaultCategoryNames()}: }{Returns all
  default category names.} \item{\code{getGeneListNames()}:
  }{Returns submitted gene list names.}
  \item{\code{getBackgroundListNames()}: }{Returns
  submitted background names.}
  \item{\code{getListName(listType=c("Gene", "Background"),
  position=1L)}:}{Get the name of the selected list type at
  a given position.} \item{\code{getSpecieNames()}:
  }{Return specie/s of the current gene list.}
  \item{\code{getCurrentGeneListPosition()}: }{Return the
  position of current gene list.}
  \item{\code{getCurrentBackgroundListPosition()}: }{Return
  the position of current background list.}
  \item{\code{getCurrentSpeciesPosition()}: }{Return
  current specie/s used positions for the uploaded gene
  list.} \item{\code{setCurrentGeneListPosition(position)}:
  }{Use the gene list of the given position.}
  \item{\code{setCurrentBackgroundPosition(position)}:
  }{Use the gene list of the given position.}
  \item{\code{setCurrentSpecies(species)}: }{Select the
  specie/s of the submitted gene list to use in the
  analysis.}
  \item{\code{setAnnotationCategories(categories)}:
  }{Select the specie/s of the submitted gene list to use
  in the analysis.} \item{\code{addList(inputIds, idType,
  listName, listType=c("Gene", "Background"))}: }{Add a
  gene or background to the current session.}
  \item{\code{getGeneCategoriesReport()}: }{Get the gene
  report categories.} \item{\code{getAnnotationSummary()}:
  }{Generate the summary of all available annotation in
  DAVID in terms of percentage of gene list ids present in
  the category and numbers of terms where the can be
  found.} \item{\code{getGeneListReportFile(fileName)}:
  }{Generate the Gene List Report a.k.a Show Gene List in
  DAVID website and save it into a file.}
  \item{\code{getGeneListReport()}: }{getGeneListReport but
  as an R object.}
  \item{\code{getFunctionalAnnotationChartFile(fileName,
  threshold=0.1, count=2L)}: }{Generate the Functional
  Annotation Chart Report for the selected functional
  categories, for the given EASE threshold and number of
  genes and save it to a file.}
  \item{\code{getFunctionalAnnotationChart(...)}:}{getFunctionalAnnotationChart
  but as an R object.}
  \item{\code{getClusterReportFile(fileName, type=c("Term",
  "Gene"), overlap=4L, initialSeed=4L, finalSeed=4L,
  linkage=0.5, kappa=35L)}:}{Generate the Term/Gene Cluster
  Report for the given configuration.}
  \item{\code{getClusterReport(type=c("Term", "Gene"),
  ...)}: }{Wrapper for getClusterReportFile function.}
  \item{\code{getFunctionalAnnotationTableFile(fileName)}:
  }{Generate Functional Annotation Table Report File, which
  is a gene-centric view of the genes and their associated
  annotation terms (selected only). There is no statistics
  applied in this report.}
  \item{\code{getFunctionalAnnotationTable()}:
  }{getFunctionalAnnotationTable but as an R object.} }
}

\section{Limitations}{
  \enumerate{ \item A job with more than 3000 genes to
  generate gene or term cluster report will not be handled
  by DAVID due to resource limit. \item No more than 200
  jobs in a day from one user or computer. \item DAVID Team
  reserves right to suspend any improper uses of the web
  service without notice. }
}
\author{
  Cristobal Fresno \email{cristobalfresno@gmail.com} and
  Elmer A. Fernandez \email{elmerfer@gmail.com}
}
\references{
  \enumerate{ \item The Database for Annotation,
  Visualization and Integrated Discovery
  (\url{david.abcc.ncifcrf.gov}) \item Huang, D. W.;
  Sherman, B. T.; Tan, Q.; Kir, J.; Liu, D.; Bryant, D.;
  Guo, Y.; Stephens, R.; Baseler, M. W.; Lane, H. C. &
  Lempicki, R. A. DAVID Bioinformatics Resources: expanded
  annotation database and novel algorithms to better
  extract biology from large gene lists. Nucleic Acids Res,
  Laboratory of Immunopathogenesis and Bioinformatics,
  SAIC-Frederick, Inc., National Cancer Institute at
  Frederick, MD 21702, USA., 2007, 35, W169-W175 \item
  Huang, D. W.; Sherman, B. T. & Lempicki, R. A.
  Bioinformatics enrichment tools: paths toward the
  comprehensive functional analysis of large gene lists.
  Nucleic Acids Res, Laboratory of Immunopathogenesis and
  Bioinformatics, Clinical Services Program,
  SAIC-Frederick, Inc., National Cancer Institute at
  Frederick, Frederick, MD 21702, USA., 2009, 37, 1-13
  \item Xiaoli Jiao, Brad T. Sherman, Da Wei Huang, Robert
  Stephens, Michael W. Baseler, H. Clifford Lane, Richard
  A. Lempicki, DAVID-WS: A Stateful Web Service to
  Facilitate Gene/Protein List Analysis Bioinformatics 2012
  \url{doi:10.1093/bioinformatics/bts251} }
}
\seealso{
  Other DAVIDWebService: \code{\link{addList}},
  \code{\link{addList}}, \code{\link{connect}},
  \code{\link{connect}},
  \code{\link{getAllAnnotationCategoryNames}},
  \code{\link{getAllAnnotationCategoryNames}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getBackgroundListNames}},
  \code{\link{getBackgroundListNames}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReportFile}},
  \code{\link{getClusterReportFile}},
  \code{\link{getCurrentBackgroundListPosition}},
  \code{\link{getCurrentBackgroundListPosition}},
  \code{\link{getCurrentGeneListPosition}},
  \code{\link{getCurrentGeneListPosition}},
  \code{\link{getCurrentSpeciesPosition}},
  \code{\link{getCurrentSpeciesPosition}},
  \code{\link{getDefaultCategoryNames}},
  \code{\link{getDefaultCategoryNames}},
  \code{\link{getEmail}}, \code{\link{getEmail}},
  \code{\link{getFunctionalAnnotationChart}},
  \code{\link{getFunctionalAnnotationChart}},
  \code{\link{getFunctionalAnnotationChartFile}},
  \code{\link{getFunctionalAnnotationChartFile}},
  \code{\link{getFunctionalAnnotationTable}},
  \code{\link{getFunctionalAnnotationTable}},
  \code{\link{getFunctionalAnnotationTableFile}},
  \code{\link{getFunctionalAnnotationTableFile}},
  \code{\link{getGeneCategoriesReport}},
  \code{\link{getGeneCategoriesReport}},
  \code{\link{getGeneListNames}},
  \code{\link{getGeneListNames}},
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getIdTypes}}, \code{\link{getIdTypes}},
  \code{\link{getListName}}, \code{\link{getListName}},
  \code{\link{getSpecieNames}},
  \code{\link{getSpecieNames}}, \code{\link{getStub}},
  \code{\link{getStub}}, \code{\link{is.connected}},
  \code{\link{is.connected}},
  \code{\link{setAnnotationCategories}},
  \code{\link{setAnnotationCategories}},
  \code{\link{setCurrentBackgroundPosition}},
  \code{\link{setCurrentBackgroundPosition(position)}},
  \code{\link{setCurrentGeneListPosition}},
  \code{\link{setCurrentGeneListPosition}},
  \code{\link{setCurrentSpecies}},
  \code{\link{setCurrentSpecies}}, \code{\link{setEmail}},
  \code{\link{setEmail}},
  \code{\link{setEmail,DAVIDWebService-method}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}
}
\keyword{DAVID}
\keyword{MEA}
\keyword{SEA}

