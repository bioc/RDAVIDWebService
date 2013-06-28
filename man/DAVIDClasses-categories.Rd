\docType{methods}
\name{categories}
\alias{categories,DAVIDFunctionalAnnotationChart-method}
\alias{categories,DAVIDFunctionalAnnotationTable-method}
\alias{categories}
\title{\code{categories} for the different DAVIDWebService package class objects}
\usage{
  categories(object)

  \S4method{categories}{DAVIDFunctionalAnnotationChart}(object)

  \S4method{categories}{DAVIDFunctionalAnnotationTable}(object)
}
\arguments{
  \item{object}{DAVIDWebService class object. Possible
  values are: DAVIDFunctionalAnnotationChart or
  DAVIDFunctionalAnnotationTable.}
}
\value{
  according to the call, one of the following objects can
  be returned: \item{DAVIDFunctionalAnnotationChart}{factor
  vector of the "Category" column.}
  \item{DAVIDFunctionalAnnotationTable}{character vector
  with the name of available main categories in the
  dictionary/membership.}
}
\description{
  Obtain ids related information, according to the given
  function call (see Values).
}
\examples{
{
##DAVIDFunctionalAnnotationChart example
##Load the Functional Annotation Chart file report for the input demo
##file 2, using data function. Then, create a DAVIDFunctionalAnnotationChart
##object using the loaded data.frame funChart2.
data(funChart2)
davidFunChart2<-DAVIDFunctionalAnnotationChart(funChart2)

##In Addition to the usual data.frame accessors, the user can inspect the
##main categories used in the analysis.
categories(davidFunChart2)

##DAVIDFunctionalAnnotationTable example
##Load the Functional Annotation Table file report for the input demo
##file 1, using data function. Then, create a DAVIDFunctionalAnnotationTable
##object using the loaded data.frame annotationTable1.
data(annotationTable1)
davidFunTable1<-DAVIDFunctionalAnnotationTable(annotationTable1)

##Now, the user can inspect the main categories used in the analysis.
categories(davidFunTable1)
}
}
\author{
  Cristobal Fresno and Elmer A Fernandez
}
\seealso{
  Other DAVIDFunctionalAnnotationChart:
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart-class}},
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
  \code{\link{dictionary}}, \code{\link{dictionary}},
  \code{\link{genes}}, \code{\link{genes}},
  \code{\link{genes}}, \code{\link{genes}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{membership}},
  \code{\link{membership}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{subset}},
  \code{\link{subset}}
}

