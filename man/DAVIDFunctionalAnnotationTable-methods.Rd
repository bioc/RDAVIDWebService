\docType{methods}
\name{subset}
\alias{dictionary,DAVIDFunctionalAnnotationTable-method}
\alias{dictionary-methods}
\alias{membership,DAVIDFunctionalAnnotationTable-method}
\alias{membership-methods}
\alias{subset,DAVIDFunctionalAnnotationTable-method}
\alias{dictionary}
\alias{membership}
\alias{subset}
\title{Methods for \code{DAVIDFunctionalAnnotationTable} class object}
\usage{
  subset(x, ...)

  \S4method{subset}{DAVIDFunctionalAnnotationTable}(x,selection=c("Membership",
  "Dictionary"), category, drop=TRUE)

  dictionary(object, ...)

  \S4method{dictionary}{DAVIDFunctionalAnnotationTable}(object,
  ...)

  membership(object, ...)

  \S4method{membership}{DAVIDFunctionalAnnotationTable}(object,
  ...)
}
\arguments{
  \item{object,x}{DAVIDFunctionalAnnotationTable class
  object.}

  \item{selection}{which slot to use to obtain the subset.
  Possible values are "Membership" or "Dictionary".}

  \item{category}{named list with main annotation category,
  which contains a character vector with the ids to use.
  Default value is missing in order to use all available
  categories of the report.}

  \item{drop}{Should list structure be drop if length==1?
  Default value TRUE.}

  \item{...}{Additional parameters for subset function
  call.}
}
\value{
  according to the call one of the following objects can be
  returned \item{subset}{list with filtered categories/ids
  according to function call.} \item{enrichment}{numeric
  vector with DAVID cluster's enrichment score.}
  \item{members}{list with DAVID Cluster's members.}
}
\description{
  Obtain DAVIDFunctionalAnnotationTable related
  information, according to the given function call (see
  Values).
}
\examples{
{
##Load the Functional Annotation Table file report for the input demo
##file 1, using data function. Then, create a DAVIDFunctionalAnnotationTable
##object using the loaded data.frame annotationTable1.
data(annotationTable1)
davidFunTable1<-DAVIDFunctionalAnnotationTable(annotationTable1)

##Obtain the head of the dictionary and the membership matrix for the first
##annotated genes used in davidFunTable1 object.
head(membership(davidFunTable1, categories(davidFunTable1)[1]))
head(dictionary(davidFunTable1, categories(davidFunTable1)[1]))
head(genes(davidFunTable1))
}
}
\author{
  Cristobal Fresno and Elmer A Fernandez
}
\seealso{
  Other DAVIDCluster: \code{\link{DAVIDCluster-class}},
  \code{\link{cluster}}, \code{\link{cluster}},
  \code{\link{enrichment}}, \code{\link{enrichment}},
  \code{\link{members}}, \code{\link{members}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}}

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
  \code{\link{categories}}, \code{\link{categories}},
  \code{\link{categories}}, \code{\link{genes}},
  \code{\link{genes}}, \code{\link{genes}},
  \code{\link{genes}}, \code{\link{initialize}},
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
  \code{\link{categories}}, \code{\link{categories}},
  \code{\link{categories}}, \code{\link{genes}},
  \code{\link{genes}}, \code{\link{genes}},
  \code{\link{genes}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}}
}

