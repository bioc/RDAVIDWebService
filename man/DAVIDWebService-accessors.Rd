\docType{methods}
\name{setEmail}
\alias{getEmail,DAVIDWebService-method}
\alias{getStub,DAVIDWebService-method}
\alias{setEmail,DAVIDWebService-method}
\alias{setEmail,character-method}
\alias{getEmail}
\alias{getStub}
\alias{setEmail}
\alias{setEmail,DAVIDWebService-method}
\title{Accessor methods for \code{DAVIDWebService} class}
\usage{
  setEmail(object, mail)

  \S4method{setEmail}{DAVIDWebService}(object, mail)

  \S4method{setEmail}{character}(mail)

  getEmail(object)

  \S4method{getEmail}{DAVIDWebService}(object)

  getStub(object)

  \S4method{getStub}{DAVIDWebService}(object)
}
\arguments{
  \item{object}{DAVIDWebService class object.}

  \item{mail}{character with a registered e-mail account at
  DAVID's website.}
}
\value{
  according to the call one of the following objects can be
  returned \item{setEmail}{character with the given e-mail
  to set.} \item{getEmail}{character with the e-mail under
  use.} \item{getstub}{jobjRef object with the stub java
  object to interface with DAVID API.}
}
\description{
  Setter/getters for \code{DAVIDWebService} class fields.
}
\details{
  Note that \code{DAVIDWebService} is a Reference class,
  hence invoke it using
  object_name$setter/getter(parameters). In addition, the
  user can use the S4 version style function call.
}
\examples{
{
##Create a DAVIDWebService object
david<-DAVIDWebService$new()

##Invoke Reference class style function setter/getters
david$setEmail("valid_mail@david.org")
david$getEmail()
stub<-david$getStub()

##Or the equivalent S4 style function call setter/getters
setEmail(david, "valid_mail@david.org")
getEmail(david)
stub<-getStub(david)
}
}
\references{
  \enumerate{ \item DAVID web
  \url{http://david.abcc.ncifcrf.gov} \item DAVID API
  \url{http://david.abcc.ncifcrf.gov/content.jsp?file=WS.html}
  }
}
\seealso{
  Other DAVIDWebService:
  \code{\link{DAVIDWebService-class}},
  \code{\link{addList}}, \code{\link{addList}},
  \code{\link{connect}}, \code{\link{connect}},
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
  \code{\link{getSpecieNames}}, \code{\link{is.connected}},
  \code{\link{is.connected}},
  \code{\link{setAnnotationCategories}},
  \code{\link{setAnnotationCategories}},
  \code{\link{setCurrentBackgroundPosition}},
  \code{\link{setCurrentBackgroundPosition(position)}},
  \code{\link{setCurrentGeneListPosition}},
  \code{\link{setCurrentGeneListPosition}},
  \code{\link{setCurrentSpecies}},
  \code{\link{setCurrentSpecies}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}
}

