\docType{methods}
\name{summary}
\alias{summary,DAVIDCluster-method}
\alias{summary,DAVIDGODag-method}
\alias{summary,DAVIDWebService-method}
\alias{summary}
\title{Basic summary for DAVIDWebService package classes.}
\usage{
  summary(object, ...)

  \S4method{summary}{DAVIDCluster}(object)

  \S4method{summary}{DAVIDGODag}(object, ...)

  \S4method{summary}{DAVIDWebService}(object)
}
\arguments{
  \item{object}{DAVIDXX class members (where XX stands for
  Term/GeneCluster, GODag or DAVIDWebService).}

  \item{...}{Additional parameters.}
}
\value{
  data.frame with summary output.
}
\description{
  The different implementations of summary function for the
  DAVIDWebService package classes.
}
\examples{
{
##DAVIDGODag example:
##Load the Functional Annotation Chart file report for the input demo
##file 2, using data function. Then, create a DAVIDGODag object using
##Molecular Function main category of DAVIDFunctionalAnnotationChart object,
##obtained from the loaded data.frame funChart2. In addition, we have
##selected a threshold pvalue of 0.001 and removed unattached nodes, in case
##DAVID/GO.db database are not using the same version.
data(funChart2)
davidGODag<-DAVIDGODag(DAVIDFunctionalAnnotationChart(funChart2), type="MF",
pvalueCutoff=0.001, removeUnattached=TRUE)
summary(davidGODag)


##DAVIDGeneCluster example:
##Load the Gene Functional Classification Tool file report for the
##input demo list 1 file to create a DAVIDGeneCluster object.
setwd(tempdir())
fileName<-system.file("files/geneClusterReport1.tab.tar.gz",
package="RDAVIDWebService")
untar(fileName)
davidGeneCluster1<-DAVIDGeneCluster(untar(fileName, list=TRUE))
davidGeneCluster1

##Now we can invoke DAVIDCluster ancestor functions to inspect the report
##data, of each cluster. For example, we can call summary to get a general
##idea
summary(davidGeneCluster1)


##DAVIDTermCluster example:
##Load the Gene Functional Classification Tool file report for the
##input demo file 2 to create a DAVIDGeneCluster object.
setwd(tempdir())
fileName<-system.file("files/termClusterReport2.tab.tar.gz",
package="RDAVIDWebService")
untar(fileName)
davidTermCluster2<-DAVIDTermCluster(untar(fileName, list=TRUE))
davidTermCluster2

##Now we can invoke DAVIDCluster ancestor functions to inspect the report
##data, of each cluster. For example, we can call summary to get a general
##idea
summary(davidTermCluster2)
}
}
\author{
  Cristobal Fresno and Elmer A Fernandez
}
\seealso{
  Other DAVIDCluster: \code{\link{DAVIDCluster-class}},
  \code{\link{cluster}}, \code{\link{cluster}},
  \code{\link{dictionary}}, \code{\link{dictionary}},
  \code{\link{enrichment}}, \code{\link{enrichment}},
  \code{\link{members}}, \code{\link{members}},
  \code{\link{membership}}, \code{\link{membership}},
  \code{\link{subset}}, \code{\link{subset}}

  Other DAVIDGODag:
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDGODag}}, \code{\link{DAVIDGODag}},
  \code{\link{DAVIDGODag-class}},
  \code{\link{DAVIDGeneCluster}},
  \code{\link{DAVIDGeneCluster}}, \code{\link{DAVIDGenes}},
  \code{\link{DAVIDGenes}}, \code{\link{DAVIDGenes}},
  \code{\link{DAVIDTermCluster}},
  \code{\link{DAVIDTermCluster}}, \code{\link{as}},
  \code{\link{as}}, \code{\link{as}},
  \code{\link{benjaminis}}, \code{\link{benjaminis}},
  \code{\link{bonferronis}}, \code{\link{bonferronis}},
  \code{\link{counts}}, \code{\link{counts}},
  \code{\link{fdrs}}, \code{\link{fdrs}},
  \code{\link{foldEnrichments}},
  \code{\link{foldEnrichments}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{listTotals}}, \code{\link{listTotals}},
  \code{\link{percentages}}, \code{\link{percentages}},
  \code{\link{popHits}}, \code{\link{popHits}},
  \code{\link{popTotals}}, \code{\link{popTotals}},
  \code{\link{terms}}, \code{\link{terms}},
  \code{\link{universeCounts}},
  \code{\link{universeMappedCount}},
  \code{\link{upsideDown}}, \code{\link{upsideDown}}

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
  \code{\link{setEmail,DAVIDWebService-method}}
}

