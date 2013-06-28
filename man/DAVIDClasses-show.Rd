\docType{methods}
\name{show}
\alias{show,DAVIDCluster-method}
\alias{show,DAVIDFunctionalAnnotationChart-method}
\alias{show,DAVIDFunctionalAnnotationTable-method}
\alias{show,DAVIDGenes-method}
\alias{show,DAVIDResult-method}
\alias{show,DAVIDWebService-method}
\alias{show}
\title{Basic console output}
\usage{
  \S4method{show}{DAVIDResult}(object)

  \S4method{show}{DAVIDGenes}(object)

  \S4method{show}{DAVIDFunctionalAnnotationChart}(object)

  \S4method{show}{DAVIDCluster}(object)

  \S4method{show}{DAVIDFunctionalAnnotationTable}(object)

  \S4method{show}{DAVIDWebService}(object)
}
\arguments{
  \item{object}{DAVIDXX class members (where XX stands for
  Result, Genes, Term/GeneCluster,
  FunctionalAnnotationChart/Table or DAVIDWebService).}
}
\value{
  Basic console output.
}
\description{
  The different implementations of show function for the
  DAVIDWebService package classes.
}
\examples{
{
##DAVIDGenes example:
##Load Show Gene List file report for the input demo file 1, using data
##function. Then, create a DAVIDGenes object using only the head of the
##loaded data.frame geneList1 (just to keep it simple).
data(geneList1)
davidGenes1<-DAVIDGenes(head(geneList1))
davidGenes1

##DAVIDFunctionalAnnotationChart example
##Load the Functional Annotation Chart file report for the input demo
##file 2, using data function. Then, create a DAVIDFunctionalAnnotationChart
##object using the head of the loaded data.frame funChart2 (just to keep
##it simple).
data(funChart2)
davidFunChart2<-DAVIDFunctionalAnnotationChart(head(funChart2))
davidFunChart2

##DAVIDFunctionalAnnotationTable example:
##Load the Functional Annotation Table file report for the input demo
##file 1, using data function. Then, create a DAVIDFunctionalAnnotationTable
##object using the loaded data.frame annotationTable1.
data(annotationTable1)
davidFunTable1<-DAVIDFunctionalAnnotationTable(annotationTable1)
davidFunTable1
}
}
\author{
  Cristobal Fresno and Elmer A Fernandez
}

