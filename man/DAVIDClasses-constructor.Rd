\docType{methods}
\name{DAVIDGenes}
\alias{DAVIDFunctionalAnnotationChart,character-method}
\alias{DAVIDFunctionalAnnotationChart,data.frame-method}
\alias{DAVIDFunctionalAnnotationChart-methods}
\alias{DAVIDFunctionalAnnotationTable,character-method}
\alias{DAVIDFunctionalAnnotationTable,data.frame-method}
\alias{DAVIDFunctionalAnnotationTable-methods}
\alias{DAVIDGODag,DAVIDFunctionalAnnotationChart-method}
\alias{DAVIDGODag-methods}
\alias{DAVIDGeneCluster,character-method}
\alias{DAVIDGeneCluster-methods}
\alias{DAVIDGenes,character-method}
\alias{DAVIDGenes,data.frame-method}
\alias{DAVIDGenes-methods}
\alias{DAVIDTermCluster,character-method}
\alias{DAVIDTermCluster-methods}
\alias{initialize,DAVIDCluster-method}
\alias{initialize,DAVIDFunctionalAnnotationChart-method}
\alias{initialize,DAVIDFunctionalAnnotationTable-method}
\alias{initialize,DAVIDGODag-method}
\alias{initialize,DAVIDGeneCluster-method}
\alias{initialize,DAVIDGenes-method}
\alias{initialize,DAVIDTermCluster-method}
\alias{DAVIDFunctionalAnnotationChart}
\alias{DAVIDFunctionalAnnotationTable}
\alias{DAVIDGODag}
\alias{DAVIDGeneCluster}
\alias{DAVIDGenes}
\alias{DAVIDTermCluster}
\alias{as}
\alias{initialize}
\title{High level constructors for DAVIDWebService package's classes.}
\usage{
  DAVIDGenes(object)

  \S4method{DAVIDGenes}{character}(object)

  \S4method{DAVIDGenes}{data.frame}(object)

  \S4method{initialize}{DAVIDGenes}(.Object, fileName)

  as(object, Class, strict=TRUE,
  ext=possibleExtends(thisClass, Class))

  DAVIDFunctionalAnnotationChart(object)

  \S4method{DAVIDFunctionalAnnotationChart}{character}(object)

  \S4method{DAVIDFunctionalAnnotationChart}{data.frame}(object)

  \S4method{initialize}{DAVIDFunctionalAnnotationChart}(.Object,
  fileName)

  as(object, Class, strict=TRUE,
  ext=possibleExtends(thisClass, Class))

  \S4method{initialize}{DAVIDCluster}(.Object, fileName)

  \S4method{initialize}{DAVIDGeneCluster}(.Object,
  fileName)

  DAVIDGeneCluster(object)

  \S4method{DAVIDGeneCluster}{character}(object)

  \S4method{initialize}{DAVIDTermCluster}(.Object,
  fileName)

  DAVIDTermCluster(object)

  \S4method{DAVIDTermCluster}{character}(object)

  \S4method{initialize}{DAVIDFunctionalAnnotationTable}(.Object,
  fileName)

  as(object, Class, strict=TRUE,
  ext=possibleExtends(thisClass, Class))

  DAVIDFunctionalAnnotationTable(object)

  \S4method{DAVIDFunctionalAnnotationTable}{character}(object)

  \S4method{DAVIDFunctionalAnnotationTable}{data.frame}(object)

  \S4method{initialize}{DAVIDGODag}(.Object,funChart,type=c("BP","MF","CC"),pvalueCutoff=0.1,removeUnattached=FALSE,...)

  DAVIDGODag(funChart, ...)

  \S4method{DAVIDGODag}{DAVIDFunctionalAnnotationChart}(funChart,
  ...)
}
\arguments{
  \item{object}{could be a character with the file name of
  the .tab report or data.frame already loaded.}

  \item{fileName}{character with the file name of the .tab
  report to load.}

  \item{.Object}{character to use in new function call.
  Possible values are: "DAVIDGenes",
  "DAVIDFunctionalAnnotationChart" or "DAVIDCluster".}

  \item{Class}{character to use in the \code{\link{as}}
  function call. Possible values are: "DAVIDGenes" and
  "DAVIDFunctionalAnnotationChart".}

  \item{strict,ext}{see \code{\link{as}} function.}

  \item{funChart}{DAVIDFunctionalAnnotationChart object.}

  \item{type}{character to indicate Gene Ontology main
  category: "BP", "MF" or "CC".}

  \item{pvalueCutoff}{numeric >0 <=1 to indicate the
  p-value to use as the threshold for enrichment. Default
  value is 0.1}

  \item{removeUnattached}{Should unattached nodes be
  removed from GO DAG? Default value is FALSE.}

  \item{...}{Additional parameters for lower level
  constructors (initialize).}
}
\value{
  a DAVIDWebService object according to function call:
  \item{DAVIDGenes}{object with genes description related
  data.} \item{DAVIDFunctionalAnnotationChart}{object with
  the respective report.}
  \item{DAVIDFunctionalAnnotationTable}{object with the
  respective report.} \item{DAVIDCluster}{Not possible to
  invoke as it is a Virtual class.}
  \item{DAVIDGeneCluster}{object with the respective
  report.} \item{DAVIDTermCluster}{object with the
  respective report.} \item{DAVIDGODag}{derived GOstats GO
  Direct Acyclic Graph from DAVIDFunctionalAnnotationChart
  data.}
}
\description{
  Different ways to build the different DAVIDWebService's
  object according to the signature in use.
}
\examples{
{
##DAVIDGenes example:
##Load Show Gene List file report for the input demo file 1, using data
##function. Then, create a DAVIDGenes object using the loaded data.frame
##geneList1.
data(geneList1)
davidGenes1<-DAVIDGenes(geneList1)

##In addition, the user can use the file name of the downloaded file report.
##Here, we need to first uncompressed the report included in the package, in
##order to load it.
setwd(tempdir())
fileName<-system.file("files/geneListReport1.tab.tar.gz",
package="RDAVIDWebService")
untar(fileName)
davidGenes1<-DAVIDGenes(untar(fileName,list=TRUE))


##DAVIDFunctionalAnnotationChart example
##Load the Functional Annotation Chart file report for the input demo
##file 2, using data function. Then, create a DAVIDFunctionalAnnotationChart
## object using the loaded data.frame funChart2.
data(funChart2)
davidFunChart2<-DAVIDFunctionalAnnotationChart(funChart2)

##In addition, the user can use the file name of the downloaded file report.
##Here, we need to first uncompressed the report included in the package, in
##order to load it.
setwd(tempdir())
fileName<-system.file("files/functionalAnnotationChartReport2.tab.tar.gz",
package="RDAVIDWebService")
untar(fileName)
davidFunChart2<-DAVIDFunctionalAnnotationChart(untar(fileName, list=TRUE))


##DAVIDFunctionalAnnotationTable example
##Load the Functional Annotation Table file report for the input demo
##file 1, using data function. Then, create a DAVIDFunctionalAnnotationTable
##object using the loaded data.frame annotationTable1.
data(annotationTable1)
davidFunTable1<-DAVIDFunctionalAnnotationTable(annotationTable1)

##In addition, the user can use the file name of the downloaded file report.
##Here, we need to first uncompressed the report included in the package, in
##order to load it.
setwd(tempdir())
fileName<-system.file("files/annotationTableReport1.tab.tar.gz",
package="RDAVIDWebService")
untar(fileName)
davidFunTable1<-DAVIDFunctionalAnnotationTable(untar(fileName, list=TRUE))


##Example DAVIDGODag
##Load the Functional Annotation Chart file report for the input demo
##file 2, using data function. Then, create a DAVIDGODag object using
##Molecular Function main category of DAVIDFunctionalAnnotationChart object,
##obtained from the loaded data.frame funChart2. In addition, we have
##selected a threshold pvalue of 0.001 and removed unattached nodes, in case
##DAVID/GO.db database are not using the same version.
data(funChart2)
davidGODag<-DAVIDGODag(DAVIDFunctionalAnnotationChart(funChart2), type="MF",
pvalueCutoff=0.001, removeUnattached=TRUE)


##DAVIDGeneCluster example:
##Load the Gene Functional Classification Tool file report for the
##input demo list 1 file to create a DAVIDGeneCluster object.
setwd(tempdir())
fileName<-system.file("files/geneClusterReport1.tab.tar.gz",
package="RDAVIDWebService")
untar(fileName)
davidGeneCluster1<-DAVIDGeneCluster(untar(fileName, list=TRUE))


##DAVIDTermCluster example:
##Load the Gene Functional Classification Tool file report for the
##input demo file 2 to create a DAVIDGeneCluster object.
setwd(tempdir())
fileName<-system.file("files/termClusterReport2.tab.tar.gz",
package="RDAVIDWebService")
untar(fileName)
davidTermCluster2<-DAVIDTermCluster(untar(fileName, list=TRUE))
}
}
\author{
  Cristobal Fresno and Elmer A Fernandez
}
\seealso{
  Other DAVIDFunctionalAnnotationChart:
  \code{\link{DAVIDFunctionalAnnotationChart-class}},
  \code{\link{categories}}, \code{\link{categories}},
  \code{\link{categories}}, \code{\link{ids}},
  \code{\link{ids}}, \code{\link{ids}}, \code{\link{ids}},
  \code{\link{ids}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}

  Other DAVIDFunctionalAnnotationTable:
  \code{\link{DAVIDFunctionalAnnotationTable-class}},
  \code{\link{categories}}, \code{\link{categories}},
  \code{\link{categories}}, \code{\link{dictionary}},
  \code{\link{dictionary}}, \code{\link{genes}},
  \code{\link{genes}}, \code{\link{genes}},
  \code{\link{genes}}, \code{\link{membership}},
  \code{\link{membership}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{subset}},
  \code{\link{subset}}

  Other DAVIDGODag: \code{\link{DAVIDGODag-class}},
  \code{\link{benjaminis}}, \code{\link{benjaminis}},
  \code{\link{bonferronis}}, \code{\link{bonferronis}},
  \code{\link{counts}}, \code{\link{counts}},
  \code{\link{fdrs}}, \code{\link{fdrs}},
  \code{\link{foldEnrichments}},
  \code{\link{foldEnrichments}}, \code{\link{listTotals}},
  \code{\link{listTotals}}, \code{\link{percentages}},
  \code{\link{percentages}}, \code{\link{popHits}},
  \code{\link{popHits}}, \code{\link{popTotals}},
  \code{\link{popTotals}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{terms}},
  \code{\link{terms}}, \code{\link{universeCounts}},
  \code{\link{universeMappedCount}},
  \code{\link{upsideDown}}, \code{\link{upsideDown}}

  Other DAVIDGeneCluster:
  \code{\link{DAVIDGeneCluster-class}},
  \code{\link{genes}}, \code{\link{genes}},
  \code{\link{genes}}, \code{\link{genes}},
  \code{\link{ids}}, \code{\link{ids}}, \code{\link{ids}},
  \code{\link{ids}}, \code{\link{ids}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}}

  Other DAVIDGenes: \code{\link{DAVIDGenes-class}},
  \code{\link{genes}}, \code{\link{genes}},
  \code{\link{genes}}, \code{\link{genes}},
  \code{\link{ids}}, \code{\link{ids}}, \code{\link{ids}},
  \code{\link{ids}}, \code{\link{ids}}

  Other DAVIDTermCluster:
  \code{\link{DAVIDTermCluster-class}}, \code{\link{ids}},
  \code{\link{ids}}, \code{\link{ids}}, \code{\link{ids}},
  \code{\link{ids}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}
}

