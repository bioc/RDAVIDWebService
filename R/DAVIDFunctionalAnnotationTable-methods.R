#' Methods for \code{DAVIDFunctionalAnnotationTable} class object
#'
#' Obtain DAVIDFunctionalAnnotationTable related information, according to the
#' given function call (see Values). 
#' 
#' @param object,x DAVIDFunctionalAnnotationTable class object.
#' @param selection which slot to use to obtain the subset. Possible values are
#' "Membership" or "Dictionary".
#' @param category named list with main annotation category, which contains a
#' character vector with the ids to use. Default value is missing in order to
#' use all available categories of the report.
#' @param drop Should list structure be drop if length==1? Default value TRUE.
#' @param ... Additional parameters for subset function call.
#'
#' @return according to the call one of the following objects can be returned
#'  \item{subset}{list with filtered categories/ids according to function call.}

#'  \item{enrichment}{numeric vector with DAVID cluster's enrichment score.}
#'  \item{members}{list with DAVID Cluster's members.}
#'  
#' @author Cristobal Fresno and Elmer A Fernandez
#'
#' @docType methods
#' @exportMethod subset
#' @name subset
#' @rdname DAVIDFunctionalAnnotationTable-methods
#' @usage subset(x, ...)
#' @aliases subset
#' @examples
#' {
#'  ##Load the Functional Annotation Table file report for the input demo 
#'  ##file 1, using data function. Then, create a DAVIDFunctionalAnnotationTable
#'  ##object using the loaded data.frame annotationTable1. 
#'  data(annotationTable1)
#'  davidFunTable1<-DAVIDFunctionalAnnotationTable(annotationTable1)
#'  
#'  ##Obtain the head of the dictionary and the membership matrix for the first
#'  ##annotated genes used in davidFunTable1 object.
#'  head(membership(davidFunTable1, categories(davidFunTable1)[1]))
#'  head(dictionary(davidFunTable1, categories(davidFunTable1)[1]))
#'  head(genes(davidFunTable1))
#' }
#'
setGeneric("subset")
#' @exportMethod subset
#' @name subset
#' @rdname DAVIDFunctionalAnnotationTable-methods
#' @usage \S4method{subset}{DAVIDFunctionalAnnotationTable}(x,selection=c("Membership", "Dictionary"), category, drop=TRUE)
#' @aliases subset,DAVIDFunctionalAnnotationTable-method
#' @family DAVIDCluster
setMethod(f="subset",signature=signature("DAVIDFunctionalAnnotationTable"),
  definition=function(x, selection=c("Membership", "Dictionary"), category,
  drop=TRUE){
    ##Check selection
    stopifnot(selection[1]%in%c("Membership", "Dictionary"))
    ans<-switch(selection[1], Membership=x@Membership, Dictionary=x@Dictionary)
  
    ##Check category
    if(!missing(category)){
      stopifnot(is.list(category)|is.character(category))
      ##character category
      if(is.character(category)){
        stopifnot(all(category%in%categories(x)))
        ans<-ans[category]
      }else{
        ##list category
        stopifnot(all(names(category)%in%categories(x)))
        ans<-lapply(names(category),function(categ){
          found<-x@Dictionary[[categ]]$ID %in% category[[categ]]
          switch(selection[1], 
            Membership=ans[[categ]][, found, drop=FALSE],
            Dictionary=ans[[categ]][found, ,drop=FALSE])
        })
      }
      ##drop?
      if(drop && length(ans)==1){
        ans<-ans[[1]]
      }
    }#!missing category

    return(ans)
})
#' @exportMethod dictionary
#' @name dictionary
#' @rdname DAVIDFunctionalAnnotationTable-methods
#' @inheritParams subset
#' @usage dictionary(object, ...)
#' @aliases dictionary-methods
setGeneric(name="dictionary", def=function(object, ...){
  standardGeneric("dictionary")
})
#'
#' @exportMethod dictionary
#' @name dictionary
#' @rdname DAVIDFunctionalAnnotationTable-methods
#' @inheritParams subset
#' @usage \S4method{dictionary}{DAVIDFunctionalAnnotationTable}(object, ...)
#' @aliases dictionary,DAVIDFunctionalAnnotationTable-method
#' @family DAVIDFunctionalAnnotationTable
setMethod(f="dictionary", signature=signature("DAVIDFunctionalAnnotationTable"),
  definition=function(object, ...){
    subset(x=object, selection="Dictionary", ...)
})
#' @exportMethod membership
#' @name membership
#' @rdname DAVIDFunctionalAnnotationTable-methods
#' @inheritParams subset
#' @usage membership(object, ...)
#' @aliases membership-methods
setGeneric(name="membership", def=function(object, ...){
  standardGeneric("membership")
})
#'
#' @exportMethod membership
#' @name membership
#' @rdname DAVIDFunctionalAnnotationTable-methods
#' @inheritParams subset
#' @usage \S4method{membership}{DAVIDFunctionalAnnotationTable}(object, ...)
#' @aliases membership,DAVIDFunctionalAnnotationTable-method
#' @family DAVIDFunctionalAnnotationTable
setMethod(f="membership", signature=signature("DAVIDFunctionalAnnotationTable"),
  definition = function(object, ...){
    subset(x=object, selection="Membership", ...)
})
