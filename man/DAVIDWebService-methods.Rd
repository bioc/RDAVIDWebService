\docType{methods}
\name{is.connected}
\alias{addList,DAVIDWebService-method}
\alias{connect,DAVIDWebService-method}
\alias{getAllAnnotationCategoryNames,DAVIDWebService-method}
\alias{getBackgroundListNames,DAVIDWebService-method}
\alias{getCurrentBackgroundListPosition,DAVIDWebService-method}
\alias{getCurrentGeneListPosition,DAVIDWebService-method}
\alias{getCurrentSpeciesPosition,DAVIDWebService-method}
\alias{getDefaultCategoryNames,DAVIDWebService-method}
\alias{getGeneListNames,DAVIDWebService-method}
\alias{getIdTypes,DAVIDWebService-method}
\alias{getListName,DAVIDWebService-method}
\alias{getSpecieNames,DAVIDWebService-method}
\alias{is.connected,DAVIDWebService-method}
\alias{setAnnotationCategories,DAVIDWebService-method}
\alias{setCurrentBackgroundPosition,DAVIDWebService-method}
\alias{setCurrentGeneListPosition,DAVIDWebService-method}
\alias{setCurrentSpecies,DAVIDWebService-method}
\alias{addList}
\alias{connect}
\alias{getAllAnnotationCategoryNames}
\alias{getBackgroundListNames}
\alias{getCurrentBackgroundListPosition}
\alias{getCurrentGeneListPosition}
\alias{getCurrentSpeciesPosition}
\alias{getDefaultCategoryNames}
\alias{getGeneListNames}
\alias{getIdTypes}
\alias{getListName}
\alias{getSpecieNames}
\alias{is.connected}
\alias{setAnnotationCategories}
\alias{setCurrentBackgroundPosition}
\alias{setCurrentBackgroundPosition(position)}
\alias{setCurrentGeneListPosition}
\alias{setCurrentSpecies}
\title{Methods to manipulate DAVID website}
\usage{
  is.connected(object)

  \S4method{is.connected}{DAVIDWebService}(object)

  connect(object)

  \S4method{connect}{DAVIDWebService}(object)

  getIdTypes(object)

  \S4method{getIdTypes}{DAVIDWebService}(object)

  addList(object, inputIds, idType, listName,
  listType=c("Gene", "Background"))

  \S4method{addList}{DAVIDWebService}(object, inputIds,
  idType, listName, listType=c("Gene", "Background"))

  getAllAnnotationCategoryNames(object)

  \S4method{getAllAnnotationCategoryNames}{DAVIDWebService}(object)

  getDefaultCategoryNames(object)

  \S4method{getDefaultCategoryNames}{DAVIDWebService}(object)

  getGeneListNames(object)

  \S4method{getGeneListNames}{DAVIDWebService}(object)

  getBackgroundListNames(object)

  \S4method{getBackgroundListNames}{DAVIDWebService}(object)

  getListName(object, listType=c("Gene", "Background"),
  position=1L)

  \S4method{getListName}{DAVIDWebService}(object,
  listType=c("Gene", "Background"), position=1L)

  getSpecieNames(object)

  \S4method{getSpecieNames}{DAVIDWebService}(object)

  getCurrentGeneListPosition(object)

  \S4method{getCurrentGeneListPosition}{DAVIDWebService}(object)

  getCurrentBackgroundListPosition(object)

  \S4method{getCurrentBackgroundListPosition}{DAVIDWebService}(object)

  getCurrentSpeciesPosition(object)

  \S4method{getCurrentSpeciesPosition}{DAVIDWebService}(object)

  setCurrentGeneListPosition(object, position)

  \S4method{setCurrentGeneListPosition}{DAVIDWebService}(object,
  position)

  setCurrentBackgroundPosition(object, position)

  \S4method{setCurrentBackgroundPosition}{DAVIDWebService}(object,
  position)

  setCurrentSpecies(object, species)

  \S4method{setCurrentSpecies}{DAVIDWebService}(object,
  species)

  setAnnotationCategories(object, categories)

  \S4method{setAnnotationCategories}{DAVIDWebService}(object,
  categories)
}
\arguments{
  \item{object}{DAVIDWebService class object.}

  \item{inputIds}{character vector with the associated
  ids.}

  \item{idType}{character with the type of submitted ids.}

  \item{listName}{character to identify the submitted
  list.}

  \item{listType}{character with the type of list (Gene,
  Background). Default value is "Gene".}

  \item{position}{integer with the position of the
  gene/background list to set.}

  \item{species}{numeric vector with the specie/s to use.}

  \item{categories}{character vector with the category
  name/s to use in the analysis.}
}
\value{
  according to the call one of the following objects can be
  returned \item{is.connected}{TRUE if user has registered
  email with DAVID knowledge base, FALSE otherwise.}
  \item{getIdTypes}{character vector with the available
  DAVID input ID type.} \item{addList}{list with two items:
  i)\code{inDavid}, a numeric with the percentage of the
  inputIds in DAVID knowledge database,
  ii)\code{unmappedIds}, a character vector with the
  unmapped ids if listType is "Gene", NA_character_
  otherwise.}
  \item{getAllAnnotationCategoryNames}{character vector
  with the available DAVID annotation categories.}
  \item{getDefaultCategoryNames}{character vector with a
  subset of the available DAVID annotation categories,
  chosen by default.} \item{getGeneListNames}{return a
  character vector with the name of the submitted gene
  list/s.} \item{getBackgroundListNames}{character vector
  with the name of the available background gene list/s for
  the submitted gene list/s.} \item{getListName}{character
  with the name of the list.}
  \item{getSpecieNames}{character vector with the specie/s
  and in brackets the number of DAVID Ids of the current
  gene list, e.g. Homo sapiens(155).}
  \item{getCurrentGeneListPosition}{integer with the
  position of current gene list if available, NA_integer_
  otherwise.}
  \item{getCurrentBackgroundListPosition}{integer with the
  position of current background list if available,
  NA_integer_ otherwise.}
  \item{getCurrentSpeciesPosition}{integer vector with the
  specie/s position under use for the gene list under use
  if available, NA_character_ otherwise.}
}
\description{
  \code{DAVIDWebService} class methods to manipulate DAVID
  website status from R. This includes different
  functionalities to set up and track the connexion, upload
  a Gene/Background list, check the species, etc. Note that
  \code{DAVIDWebService} is a Reference class, hence invoke
  it using object_name$method_name(parameters). In
  addition, the user can use the S4 version style function
  call (see Details).
}
\details{
  Available functions include: \describe{
  \item{\code{connect}:}{Try to establish a connection with
  DAVID server using the provided email.}
  \item{\code{is.connected}:}{Check if connected to DAVID
  server.} \item{\code{getIdTypes}:}{Returns all acceptable
  DAVID idTypes.} \item{\code{addList}:}{Add a gene or
  background to the current session.}
  \item{\code{getAllAnnotationCategoryNames}:}{Returns all
  available annotation category names.}
  \item{\code{getDefaultCategoryNames}:}{Returns all
  default category names.}
  \item{\code{getGeneListNames}:}{Returns all list names}
  \item{\code{getBackgroundListNames}:}{Returns background
  names.} \item{\code{getListName}:}{Get the name of the
  selected list type at a given position.}
  \item{\code{getSpecieNames}:}{Return specie/s of the
  current gene list.}
  \item{\code{getCurrentGeneListPosition}:}{Return the
  position of current gene list.}
  \item{\code{getCurrentBackgroundListPosition}:}{Return
  the position of current background list.}
  \item{\code{getCurrentSpeciesPosition}:}{Return current
  specie/s used positions for the uploaded gene list.}
  \item{\code{setCurrentGeneListPosition}:}{Use the gene
  list of the given position.}
  \item{\code{setCurrentBackgroundPosition:}}{Use the
  background list of the given position.}
  \item{\code{setCurrentSpecies}:}{Select the specie/s of
  the submitted gene list to use in the analysis.}
  \item{\code{setAnnotationCategories}:}{Let the user to
  select specific annotation categories.} }
}
\examples{
david <- DAVIDWebService$new()
david$is.connected()
##Or the equivalent S4 style function call
is.connected(david)
}
\seealso{
  Other DAVIDWebService:
  \code{\link{DAVIDWebService-class}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReportFile}},
  \code{\link{getClusterReportFile}},
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
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getStub}}, \code{\link{getStub}},
  \code{\link{setEmail}}, \code{\link{setEmail}},
  \code{\link{setEmail,DAVIDWebService-method}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}

  Other DAVIDWebService:
  \code{\link{DAVIDWebService-class}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReportFile}},
  \code{\link{getClusterReportFile}},
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
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getStub}}, \code{\link{getStub}},
  \code{\link{setEmail}}, \code{\link{setEmail}},
  \code{\link{setEmail,DAVIDWebService-method}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}

  Other DAVIDWebService:
  \code{\link{DAVIDWebService-class}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReportFile}},
  \code{\link{getClusterReportFile}},
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
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getStub}}, \code{\link{getStub}},
  \code{\link{setEmail}}, \code{\link{setEmail}},
  \code{\link{setEmail,DAVIDWebService-method}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}

  Other DAVIDWebService:
  \code{\link{DAVIDWebService-class}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReportFile}},
  \code{\link{getClusterReportFile}},
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
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getStub}}, \code{\link{getStub}},
  \code{\link{setEmail}}, \code{\link{setEmail}},
  \code{\link{setEmail,DAVIDWebService-method}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}

  Other DAVIDWebService:
  \code{\link{DAVIDWebService-class}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReportFile}},
  \code{\link{getClusterReportFile}},
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
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getStub}}, \code{\link{getStub}},
  \code{\link{setEmail}}, \code{\link{setEmail}},
  \code{\link{setEmail,DAVIDWebService-method}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}

  Other DAVIDWebService:
  \code{\link{DAVIDWebService-class}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReportFile}},
  \code{\link{getClusterReportFile}},
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
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getStub}}, \code{\link{getStub}},
  \code{\link{setEmail}}, \code{\link{setEmail}},
  \code{\link{setEmail,DAVIDWebService-method}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}

  Other DAVIDWebService:
  \code{\link{DAVIDWebService-class}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReportFile}},
  \code{\link{getClusterReportFile}},
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
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getStub}}, \code{\link{getStub}},
  \code{\link{setEmail}}, \code{\link{setEmail}},
  \code{\link{setEmail,DAVIDWebService-method}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}

  Other DAVIDWebService:
  \code{\link{DAVIDWebService-class}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReportFile}},
  \code{\link{getClusterReportFile}},
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
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getStub}}, \code{\link{getStub}},
  \code{\link{setEmail}}, \code{\link{setEmail}},
  \code{\link{setEmail,DAVIDWebService-method}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}

  Other DAVIDWebService:
  \code{\link{DAVIDWebService-class}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReportFile}},
  \code{\link{getClusterReportFile}},
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
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getStub}}, \code{\link{getStub}},
  \code{\link{setEmail}}, \code{\link{setEmail}},
  \code{\link{setEmail,DAVIDWebService-method}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}

  Other DAVIDWebService:
  \code{\link{DAVIDWebService-class}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReportFile}},
  \code{\link{getClusterReportFile}},
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
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getStub}}, \code{\link{getStub}},
  \code{\link{setEmail}}, \code{\link{setEmail}},
  \code{\link{setEmail,DAVIDWebService-method}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}

  Other DAVIDWebService:
  \code{\link{DAVIDWebService-class}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReportFile}},
  \code{\link{getClusterReportFile}},
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
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getStub}}, \code{\link{getStub}},
  \code{\link{setEmail}}, \code{\link{setEmail}},
  \code{\link{setEmail,DAVIDWebService-method}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}

  Other DAVIDWebService:
  \code{\link{DAVIDWebService-class}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReportFile}},
  \code{\link{getClusterReportFile}},
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
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getStub}}, \code{\link{getStub}},
  \code{\link{setEmail}}, \code{\link{setEmail}},
  \code{\link{setEmail,DAVIDWebService-method}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}

  Other DAVIDWebService:
  \code{\link{DAVIDWebService-class}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReportFile}},
  \code{\link{getClusterReportFile}},
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
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getStub}}, \code{\link{getStub}},
  \code{\link{setEmail}}, \code{\link{setEmail}},
  \code{\link{setEmail,DAVIDWebService-method}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}

  Other DAVIDWebService:
  \code{\link{DAVIDWebService-class}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReportFile}},
  \code{\link{getClusterReportFile}},
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
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getStub}}, \code{\link{getStub}},
  \code{\link{setEmail}}, \code{\link{setEmail}},
  \code{\link{setEmail,DAVIDWebService-method}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}

  Other DAVIDWebService:
  \code{\link{DAVIDWebService-class}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReportFile}},
  \code{\link{getClusterReportFile}},
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
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getStub}}, \code{\link{getStub}},
  \code{\link{setEmail}}, \code{\link{setEmail}},
  \code{\link{setEmail,DAVIDWebService-method}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}

  Other DAVIDWebService:
  \code{\link{DAVIDWebService-class}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReportFile}},
  \code{\link{getClusterReportFile}},
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
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getStub}}, \code{\link{getStub}},
  \code{\link{setEmail}}, \code{\link{setEmail}},
  \code{\link{setEmail,DAVIDWebService-method}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}

  Other DAVIDWebService:
  \code{\link{DAVIDWebService-class}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getAnnotationSummary}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReport}},
  \code{\link{getClusterReportFile}},
  \code{\link{getClusterReportFile}},
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
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReport}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getGeneListReportFile}},
  \code{\link{getStub}}, \code{\link{getStub}},
  \code{\link{setEmail}}, \code{\link{setEmail}},
  \code{\link{setEmail,DAVIDWebService-method}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}
}

