\docType{methods}
\name{plot2D}
\alias{plot2D,DAVIDFunctionalAnnotationChart-method}
\alias{plot2D,DAVIDFunctionalAnnotationTable-method}
\alias{plot2D,DAVIDGeneCluster-method}
\alias{plot2D,DAVIDResult-method}
\alias{plot2D,DAVIDTermCluster-method}
\alias{plot2D-methods}
\alias{plot2D}
\title{Visualization of biological relationships}
\usage{
  plot2D(object,...)

  \S4method{plot2D}{DAVIDResult}(object, dataFrame)

  \S4method{plot2D}{DAVIDFunctionalAnnotationChart}(object,color=c("FALSE"="black",
  "TRUE"="green"))

  \S4method{plot2D}{DAVIDGeneCluster}(object,color=c("FALSE"="black","TRUE"="green"),names=FALSE)

  \S4method{plot2D}{DAVIDTermCluster}(object,number=1,color=c("FALSE"="black","TRUE"=
  "green"))

  \S4method{plot2D}{DAVIDFunctionalAnnotationTable}(object,
  category, id, names.genes=FALSE,
  names.category=FALSE,color=c("FALSE"="black","TRUE"="green"))
}
\arguments{
  \item{object}{DAVIDResult heirs
  (DAVIDFunctionalAnnotationChart/Table or
  DAVIDGeneCluster/TermCluster)}

  \item{dataFrame}{data.frame with three columns (x, y and
  fill) to be used in ggplot. X(Y) is a character/factor
  with the X(Y)-axis labels and "fill" is a the color to be
  used for x-y labels.}

  \item{color}{named character vector to indicate tile
  color. Default value is c("FALSE"="black",
  "TRUE"="green").}

  \item{names}{should gene names be plotted? Default value
  is FALSE, i.e, use ids.}

  \item{number}{integer to indicate which cluster to plot.
  Default value is 1.}

  \item{category}{character vector to select the main
  annotation categories. By default is missing in order to
  use all the available ones.}

  \item{id}{character vector to indicate which gene ids to
  use. By default is missing in order to use all the
  available ones.}

  \item{names.genes,names.category}{Should genes and/or
  category names used? Default value is FALSE, i.e., use
  both ids.}

  \item{...}{Additional parameters for heirs functions.}
}
\value{
  a ggplot object if the object is not empty.
}
\description{
  \code{plot2D} uses a 2D tile ggplot to explore biological
  relationships between two variables such as annotation
  category and genes, for Functional Annotation Chart/Table
  or Term cluster results. For Gene cluster, the cluster
  number vs genes membership is plotted.
}
\examples{
{
##DAVIDFunctionalAnnotationChart example:
##Load the Functional Annotation Chart file report for the input demo
##file 2, using data function. Just to keep it simple, for the first five
##terms present in funChart2 object, create a DAVIDFunctionalAnnotationChart
##object and plot a 2D tile matrix with the reported evidence (green) or not
##(black).
data(funChart2)
plot2D(DAVIDFunctionalAnnotationChart(funChart2[1:5, ]),
color=c("FALSE"="black", "TRUE"="green"))

##DAVIDFunctionalAnnotationTable example
##Load the Functional Annotation Table file report for the input demo
##file 1, using data function. Then, create a DAVIDFunctionalAnnotationTable
##object using the loaded data.frame annotationTable1.
data(annotationTable1)
davidFunTable1<-DAVIDFunctionalAnnotationTable(annotationTable1)

##Plot the membership of only for the first six terms in this
##category, with only the genes of the first six terms with at least one
##evidence code.
##Category filtering...
categorySelection<-list(head(dictionary(davidFunTable1,
categories(davidFunTable1)[1])$ID))
names(categorySelection)<-categories(davidFunTable1)[1]

##Gene filter...
id<-membership(davidFunTable1, categories(davidFunTable1)[1])[,1:6]
id<-ids(genes(davidFunTable1))[rowSums(id)>0]

##Finally the membership tile plot
plot2D(davidFunTable1, category=categorySelection, id=id,
names.category=TRUE)

##DAVIDGeneCluster example:
##Load the Gene Functional Classification Tool file report for the
##input demo list 1 file to create a DAVIDGeneCluster object.
setwd(tempdir())
fileName<-system.file("files/geneClusterReport1.tab.tar.gz",
package="RDAVIDWebService")
untar(fileName)
davidGeneCluster1<-DAVIDGeneCluster(untar(fileName, list=TRUE))

##We can inspect a 2D tile membership plot, to visual inspect for
##overlapping of genes across the clusters. Or use an scaled version of gene
##names to see the association of gene cluster, e.g., cluster 3 is related to
##ATP genes.
plot2D(davidGeneCluster1)
plot2D(davidGeneCluster1,names=TRUE)+
theme(axis.text.y=element_text(size=rel(0.9)))

##DAVIDTermCluster example:
##Load the Gene Functional Classification Tool file report for the
##input demo file 2 to create a DAVIDGeneCluster object.
setwd(tempdir())
fileName<-system.file("files/termClusterReport2.tab.tar.gz",
package="RDAVIDWebService")
untar(fileName)
davidTermCluster2<-DAVIDTermCluster(untar(fileName, list=TRUE))

##Finally, we can inspect a 2D tile membership plot, to visual inspect for
##overlapping of genes across the term members of the selected cluster,
##e.g., the first cluster .
plot2D(davidTermCluster2, number=1)
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
  \code{\link{as}}, \code{\link{as}},
  \code{\link{categories}}, \code{\link{categories}},
  \code{\link{categories}}, \code{\link{ids}},
  \code{\link{ids}}, \code{\link{ids}}, \code{\link{ids}},
  \code{\link{ids}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}}

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
  \code{\link{categories}}, \code{\link{dictionary}},
  \code{\link{dictionary}}, \code{\link{genes}},
  \code{\link{genes}}, \code{\link{genes}},
  \code{\link{genes}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{membership}}, \code{\link{membership}},
  \code{\link{subset}}, \code{\link{subset}}

  Other DAVIDGeneCluster:
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDGODag}}, \code{\link{DAVIDGODag}},
  \code{\link{DAVIDGeneCluster}},
  \code{\link{DAVIDGeneCluster}},
  \code{\link{DAVIDGeneCluster-class}},
  \code{\link{DAVIDGenes}}, \code{\link{DAVIDGenes}},
  \code{\link{DAVIDGenes}}, \code{\link{DAVIDTermCluster}},
  \code{\link{DAVIDTermCluster}}, \code{\link{as}},
  \code{\link{as}}, \code{\link{as}}, \code{\link{genes}},
  \code{\link{genes}}, \code{\link{genes}},
  \code{\link{genes}}, \code{\link{ids}},
  \code{\link{ids}}, \code{\link{ids}}, \code{\link{ids}},
  \code{\link{ids}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}}

  Other DAVIDResult: \code{\link{DAVIDResult-class}},
  \code{\link{type}}, \code{\link{type}}

  Other DAVIDTermCluster:
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDGODag}}, \code{\link{DAVIDGODag}},
  \code{\link{DAVIDGeneCluster}},
  \code{\link{DAVIDGeneCluster}}, \code{\link{DAVIDGenes}},
  \code{\link{DAVIDGenes}}, \code{\link{DAVIDGenes}},
  \code{\link{DAVIDTermCluster}},
  \code{\link{DAVIDTermCluster}},
  \code{\link{DAVIDTermCluster-class}}, \code{\link{as}},
  \code{\link{as}}, \code{\link{as}}, \code{\link{ids}},
  \code{\link{ids}}, \code{\link{ids}}, \code{\link{ids}},
  \code{\link{ids}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}}
}

