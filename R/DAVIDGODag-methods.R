#' Methods for \code{DAVIDGODag} class object
#'
#' Obtain DAVIDGODag related information, according to the given function
#' call (see Values). 
#' 
#' @param object,x,r DAVIDGODag class object.
#' @param graph a graph object with the GO DAG structure.
#' @param ... Additional parameters (if required).
#' 
#' @return according to the call one of the following objects can be returned
#'  \item{upsideDown}{the same graph but the arcs with its directions in the
#'  other way around. Hence, plot layout would make upside down the graph.}
#'  \item{universeMappedCount, universeCounts, counts}{modifications to the
#'  corresponding GOstats/Category library functions, to keep the same behavior
#'  for DAVIDGODag objects.}
#'  \item{fdrs, benjaminis, bonferronis}{Adjusted method specific p-values for
#'  the corresponding nodes/terms.}
#'  \item{terms}{character vector with GO node names.}
#'  \item{popTotals, popHits, listTotals}{integer vector with the number of
#'  ids, to use in the EASE score calculations, when building the 2x2
#'  contingency table.}
#'  \item{percentages}{numeric vector with the percentage of the gene list ids
#'  present in the term.}
#'  \item{foldEnrichments}{numeric vector with the ratio of the two proportions
#'  for each node/term. For example, if 40/400 (i.e. 10\%) of your input genes
#'  involved in "kinase activity" and the background information is 300/30000
#'  genes (i.e. 1\%) associating with "kinase activity", roughly 10\%/1\%=10
#'  fold enrichment.}
#'  
#' @author Cristobal Fresno and Elmer A Fernandez
#'
#' @docType methods
#' @exportMethod terms
#' @name terms
#' @rdname DAVIDGODag-methods
#' @usage terms(x, ...)
#' @aliases terms
#' @family DAVIDGODag
#' @examples
#' {
#' ##Load the Functional Annotation Chart file report for the input demo 
#' ##file 2, using data function. Then, create a DAVIDGODag object using
#' ##Molecular Function main category of DAVIDFunctionalAnnotationChart object,
#' ##obtained from the loaded data.frame funChart2. In addition, we have
#' ##selected a threshold pvalue of 0.001 and removed unattached nodes, in case
#' ##DAVID/GO.db database are not using the same version.
#' data(funChart2)
#' davidGODag<-DAVIDGODag(DAVIDFunctionalAnnotationChart(funChart2), type="MF",
#'   pvalueCutoff=0.001, removeUnattached=TRUE)
#' 
#' ##Now, we can inspect the enrichment GO DAG using GOstats functionalities:
#' ##counts, pvalues, sigCategories, universeCounts, geneMappedCount, etc.
#' ##However, oddsRatios, expectedCounts and universeMappedCount are not
#' ##available because these results are not available on DAVID's Functional
#' ##Annotation Chart report. In addition geneIdUniverse are not the ones of
#' ##the universe but the ids on the category (geneIdsByCategory).
#' davidGODag
#' counts(davidGODag)
#' pvalues(davidGODag)
#' sigCategories(davidGODag, p=0.0001)
#' universeCounts(davidGODag)
#' geneMappedCount(davidGODag)
#' geneIdsByCategory(davidGODag)
#' summary(davidGODag) 
#' 
#' ##In addition, the new nodeData attributes (term, listTotal, popHit,
#' ##popTotal, foldEnrichment, bonferroni, benjamini, fdr) can be retrieved.
#' terms(davidGODag)
#' listTotals(davidGODag)
#' popHits(davidGODag)
#' popTotals(davidGODag)
#' foldEnrichments(davidGODag)
#' bonferronis(davidGODag)
#' benjaminis(davidGODag)
#' fdrs(davidGODag)
#' }
#'
setGeneric(name="terms")
#'
#' @exportMethod terms
#' @name terms
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage \S4method{terms}{DAVIDGODag}(x, ...)
#' @aliases terms,DAVIDGODag-method
setMethod(f="terms", signature=signature("DAVIDGODag"), 
  definition=function(x, ...){
    unlist(nodeData(goDag(x), attr="term")[x@pvalue.order])
})
#'
#' @exportMethod percentages
#' @name percentages
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage percentages(object)
#' @aliases percentages
setGeneric(name="percentages", def=function(object){
  standardGeneric("percentages")
})
#'
#' @exportMethod percentages
#' @name percentages
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage \S4method{percentages}{DAVIDGODag}(object)
#' @aliases percentages,DAVIDGODag-method
setMethod(f="percentages", signature=signature("DAVIDGODag"), 
  definition=function(object){
    unlist(nodeData(goDag(object), attr="percentage")[object@pvalue.order])
})
#'
#' @exportMethod listTotals
#' @name listTotals
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage listTotals(object)
#' @aliases listTotals
setGeneric(name="listTotals", def=function(object){
  standardGeneric("listTotals")
})
#'
#' @exportMethod listTotals
#' @name listTotals
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage \S4method{listTotals}{DAVIDGODag}(object)
#' @aliases listTotals,DAVIDGODag-method
setMethod(f="listTotals", signature=signature("DAVIDGODag"), 
  definition=function(object){
    unlist(nodeData(goDag(object), attr="listTotal")[object@pvalue.order])
})
#'
#' @exportMethod popHits
#' @name popHits
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage popHits(object)
#' @aliases popHits
setGeneric(name="popHits", def=function(object){standardGeneric("popHits")})
#'
#' @exportMethod popHits
#' @name popHits
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage \S4method{popHits}{DAVIDGODag}(object)
#' @aliases popHits,DAVIDGODag-method
setMethod(f="popHits", signature=signature("DAVIDGODag"), 
  definition=function(object){
    unlist(nodeData(goDag(object), attr="popHit")[object@pvalue.order])
})
#'
#' @exportMethod popTotals
#' @name popTotals
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage popTotals(object)
#' @aliases popTotals
setGeneric(name="popTotals", def=function(object){standardGeneric("popTotals")})
#'
#' @exportMethod popTotals
#' @name popTotals
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage \S4method{popTotals}{DAVIDGODag}(object)
#' @aliases popTotals,DAVIDGODag-method
setMethod(f="popTotals", signature=signature("DAVIDGODag"), 
  definition=function(object){
    unlist(nodeData(goDag(object), attr="popTotal")[object@pvalue.order])
})
#'
#' @exportMethod foldEnrichments
#' @name foldEnrichments
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage foldEnrichments(object)
#' @aliases foldEnrichments
setGeneric(name="foldEnrichments", def=function(object){
  standardGeneric("foldEnrichments")
})
#'
#' @exportMethod foldEnrichments
#' @name foldEnrichments
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage \S4method{foldEnrichments}{DAVIDGODag}(object)
#' @aliases foldEnrichments,DAVIDGODag-method
setMethod(f="foldEnrichments", signature=signature("DAVIDGODag"), 
  definition=function(object){
    unlist(nodeData(goDag(object), attr="foldEnrichment")[object@pvalue.order])
})
#'
#' @exportMethod bonferronis
#' @name bonferronis
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage bonferronis(object)
#' @aliases bonferronis
setGeneric(name="bonferronis", def=function(object){
  standardGeneric("bonferronis")
})
#'
#' @exportMethod bonferronis
#' @name bonferronis
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage \S4method{bonferronis}{DAVIDGODag}(object)
#' @aliases bonferronis,DAVIDGODag-method
setMethod(f="bonferronis", signature=signature("DAVIDGODag"), 
  definition=function(object){
    unlist(nodeData(goDag(object), attr="bonferroni")[object@pvalue.order])
})
#'
#' @exportMethod benjaminis
#' @name benjaminis
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage benjaminis(object)
#' @aliases benjaminis
setGeneric(name="benjaminis", def=function(object){
  standardGeneric("benjaminis")
})
#'
#' @exportMethod benjaminis
#' @name benjaminis
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage \S4method{benjaminis}{DAVIDGODag}(object)
#' @aliases benjaminis,DAVIDGODag-method
setMethod(f="benjaminis", signature=signature("DAVIDGODag"), 
  definition=function(object){
    unlist(nodeData(goDag(object), attr="benjamini")[object@pvalue.order])
})
#'
#' @exportMethod fdrs
#' @name fdrs
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage fdrs(object)
#' @aliases fdrs
setGeneric(name="fdrs", def=function(object){standardGeneric("fdrs")})
#'
#' @exportMethod fdrs
#' @name fdrs
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage \S4method{fdrs}{DAVIDGODag}(object)
#' @aliases fdrs,DAVIDGODag-method
setMethod(f="fdrs", signature=signature("DAVIDGODag"),
  definition=function(object){
  unlist(nodeData(goDag(object), attr="fdr")[object@pvalue.order])
})
#'
#' @exportMethod counts
#' @name counts
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage counts(object, ...)
#' @aliases counts
setGeneric(name="counts", def=function(object, ...){standardGeneric("counts")})
#'
#' @exportMethod counts
#' @name counts
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage \S4method{counts}{DAVIDGODag}(object, ...)
#' @aliases counts,DAVIDGODag-method
setMethod(f="counts", signature=signature("DAVIDGODag"), 
  definition=function(object, ...){
    unlist(nodeData(goDag(object), attr="count")[object@pvalue.order])
})
#' @exportMethod upsideDown
#' @name upsideDown
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage upsideDown(graph)
#' @aliases upsideDown
setGeneric(name="upsideDown", def=function(graph){
  standardGeneric("upsideDown")
})
#'
#' @exportMethod upsideDown
#' @name upsideDown
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage \S4method{upsideDown}{graph}(graph)
#' @aliases upsideDown,graph-method
setMethod(f="upsideDown", signature=signature("graph"), 
  definition=function(graph){
    ##Create a new graph with the same nodes and nodeData
    invertedGraph<-new("graphNEL", edgemode="directed", nodes=nodes(graph))
    invertedGraph@nodeData@defaults<-graph@nodeData@defaults
    invertedGraph@nodeData@data<-graph@nodeData@data

    ##Create the new edges in the opposite direction
    invisible(sapply(names(edges(graph)), function(nodeTo){
      invisible(sapply(edges(graph)[[nodeTo]], function(nodeFrom){
        invertedGraph<<-addEdge(nodeFrom, nodeTo, invertedGraph)
        return(NULL)
      }))
      return(NULL)
    }))
    
    ##Sanity Check of graph structure
    stopifnot(isomorphism(graph, invertedGraph)$isomorphism) 
    return(invertedGraph)
})
#'
#' @exportMethod universeCounts
#' @name universeCounts
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage \S4method{universeCounts}{DAVIDGODag}(r)
#' @aliases universeCounts,DAVIDGODag-method
setMethod(f="universeCounts", signature=signature("DAVIDGODag"), 
  definition=function(r){
    popHits(r)
})
#'
#' @exportMethod universeMappedCount
#' @name universeMappedCount
#' @rdname DAVIDGODag-methods
#' @inheritParams terms
#' @usage \S4method{universeMappedCount}{DAVIDGODag}(r)
#' @aliases universeMappedCount,DAVIDGODag-method
setMethod(f="universeMappedCount", signature=signature("DAVIDGODag"),
  definition=function(r){
    max(popTotals(r))
})
