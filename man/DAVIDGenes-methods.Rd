\docType{methods}
\name{species}
\alias{duplicateIds,DAVIDGenes-method}
\alias{duplicateIds-methods}
\alias{species,DAVIDGenes-method}
\alias{species-methods}
\alias{uniqueIds,DAVIDGenes-method}
\alias{uniqueIds-method}
\alias{duplicateIds}
\alias{species}
\alias{uniqueIds}
\title{Methods for \code{DAVIDGenes} class object}
\usage{
  species(object)

  \S4method{species}{DAVIDGenes}(object)

  duplicateIds(object, collapse = FALSE)

  \S4method{duplicateIds}{DAVIDGenes}(object,
  collapse=FALSE)

  uniqueIds(object)

  \S4method{uniqueIds}{DAVIDGenes}(object)
}
\arguments{
  \item{object}{DAVIDGenes class object.}

  \item{collapse}{logical indicating if duplicate ids
  should be grouped as a comma separated id. Default value
  is FALSE.}

  \item{...}{Additional parameters for internal functions
  (if applicable).}
}
\value{
  according to the call one of the following objects can be
  returned \item{show}{console output of the class and
  associated data.} \item{species}{character vector with
  the levels of Species if available.} \item{uniqueIds}{a
  DAVIDGenes object with only the gene names with a unique
  id.} \item{duplicateIds}{a DAVIDGenes object with only
  the gene names with at least two ids. If collapse is
  TRUE, a data.frame in where all the ids that matched the
  same gene name, are coded in comma separated style.}
}
\description{
  Obtain DAVIDGenes related information, according to the
  given function call (see Values).
}
\examples{
{
##Load Show Gene List file report for the input demo file 1, using data
##function. Then, create a DAVIDGenes object using the loaded data.frame
##geneList1. In addition, the user can use the file name of the downloaded
##file report.
data(geneList1)
davidGenes1<-DAVIDGenes(geneList1)

##Now we can inspect davidGenes1 as it was an common data.frame
head(davidGenes1)

##Additional getters for this object are also available, to obtain the
##different columns: ids, genes and species.
ids(davidGenes1)
genes(davidGenes1)
species(davidGenes1)

##Or even look up for a particular gene id, which will return only the
##matched ones.
genes(davidGenes1, ids=c("38926_at", "35367_at", "no match"))

##Obtain the genes with duplicate manufacturer ids or just the genes that
##do not have duplicate ids (uniqueIds).
duplicateIds(davidGenes1)
uniqueIds(davidGenes1)
}
}
\author{
  Cristobal Fresno and Elmer A Fernandez
}

