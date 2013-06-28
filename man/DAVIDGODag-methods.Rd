\docType{methods}
\name{terms}
\alias{benjaminis,DAVIDGODag-method}
\alias{bonferronis,DAVIDGODag-method}
\alias{counts,DAVIDGODag-method}
\alias{fdrs,DAVIDGODag-method}
\alias{foldEnrichments,DAVIDGODag-method}
\alias{listTotals,DAVIDGODag-method}
\alias{percentages,DAVIDGODag-method}
\alias{popHits,DAVIDGODag-method}
\alias{popTotals,DAVIDGODag-method}
\alias{terms,DAVIDGODag-method}
\alias{universeCounts,DAVIDGODag-method}
\alias{universeMappedCount,DAVIDGODag-method}
\alias{upsideDown,graph-method}
\alias{benjaminis}
\alias{bonferronis}
\alias{counts}
\alias{fdrs}
\alias{foldEnrichments}
\alias{listTotals}
\alias{percentages}
\alias{popHits}
\alias{popTotals}
\alias{terms}
\alias{universeCounts}
\alias{universeMappedCount}
\alias{upsideDown}
\title{Methods for \code{DAVIDGODag} class object}
\usage{
  terms(x, ...)

  \S4method{terms}{DAVIDGODag}(x, ...)

  percentages(object)

  \S4method{percentages}{DAVIDGODag}(object)

  listTotals(object)

  \S4method{listTotals}{DAVIDGODag}(object)

  popHits(object)

  \S4method{popHits}{DAVIDGODag}(object)

  popTotals(object)

  \S4method{popTotals}{DAVIDGODag}(object)

  foldEnrichments(object)

  \S4method{foldEnrichments}{DAVIDGODag}(object)

  bonferronis(object)

  \S4method{bonferronis}{DAVIDGODag}(object)

  benjaminis(object)

  \S4method{benjaminis}{DAVIDGODag}(object)

  fdrs(object)

  \S4method{fdrs}{DAVIDGODag}(object)

  counts(object, ...)

  \S4method{counts}{DAVIDGODag}(object, ...)

  upsideDown(graph)

  \S4method{upsideDown}{graph}(graph)

  \S4method{universeCounts}{DAVIDGODag}(r)

  \S4method{universeMappedCount}{DAVIDGODag}(r)
}
\arguments{
  \item{object,x,r}{DAVIDGODag class object.}

  \item{graph}{a graph object with the GO DAG structure.}

  \item{...}{Additional parameters (if required).}
}
\value{
  according to the call one of the following objects can be
  returned \item{upsideDown}{the same graph but the arcs
  with its directions in the other way around. Hence, plot
  layout would make upside down the graph.}
  \item{universeMappedCount, universeCounts,
  counts}{modifications to the corresponding
  GOstats/Category library functions, to keep the same
  behavior for DAVIDGODag objects.} \item{fdrs, benjaminis,
  bonferronis}{Adjusted method specific p-values for the
  corresponding nodes/terms.} \item{terms}{character vector
  with GO node names.} \item{popTotals, popHits,
  listTotals}{integer vector with the number of ids, to use
  in the EASE score calculations, when building the 2x2
  contingency table.} \item{percentages}{numeric vector
  with the percentage of the gene list ids present in the
  term.} \item{foldEnrichments}{numeric vector with the
  ratio of the two proportions for each node/term. For
  example, if 40/400 (i.e. 10\%) of your input genes
  involved in "kinase activity" and the background
  information is 300/30000 genes (i.e. 1\%) associating
  with "kinase activity", roughly 10\%/1\%=10 fold
  enrichment.}
}
\description{
  Obtain DAVIDGODag related information, according to the
  given function call (see Values).
}
\examples{
{
##Load the Functional Annotation Chart file report for the input demo
##file 2, using data function. Then, create a DAVIDGODag object using
##Molecular Function main category of DAVIDFunctionalAnnotationChart object,
##obtained from the loaded data.frame funChart2. In addition, we have
##selected a threshold pvalue of 0.001 and removed unattached nodes, in case
##DAVID/GO.db database are not using the same version.
data(funChart2)
davidGODag<-DAVIDGODag(DAVIDFunctionalAnnotationChart(funChart2), type="MF",
pvalueCutoff=0.001, removeUnattached=TRUE)

##Now, we can inspect the enrichment GO DAG using GOstats functionalities:
##counts, pvalues, sigCategories, universeCounts, geneMappedCount, etc.
##However, oddsRatios, expectedCounts and universeMappedCount are not
##available because these results are not available on DAVID's Functional
##Annotation Chart report. In addition geneIdUniverse are not the ones of
##the universe but the ids on the category (geneIdsByCategory).
davidGODag
counts(davidGODag)
pvalues(davidGODag)
sigCategories(davidGODag, p=0.0001)
universeCounts(davidGODag)
geneMappedCount(davidGODag)
geneIdsByCategory(davidGODag)
summary(davidGODag)

##In addition, the new nodeData attributes (term, listTotal, popHit,
##popTotal, foldEnrichment, bonferroni, benjamini, fdr) can be retrieved.
terms(davidGODag)
listTotals(davidGODag)
popHits(davidGODag)
popTotals(davidGODag)
foldEnrichments(davidGODag)
bonferronis(davidGODag)
benjaminis(davidGODag)
fdrs(davidGODag)
}
}
\author{
  Cristobal Fresno and Elmer A Fernandez
}
\seealso{
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
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{summary}},
  \code{\link{summary}}, \code{\link{summary}},
  \code{\link{summary}}
}

