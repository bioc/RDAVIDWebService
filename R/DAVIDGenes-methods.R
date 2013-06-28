#' Methods for \code{DAVIDGenes} class object
#'
#' Obtain DAVIDGenes related information, according to the given function call 
#' (see Values). 
#' 
#' @param object DAVIDGenes class object.
#' @param collapse logical indicating if duplicate ids should be grouped as a 
#' comma separated id. Default value is FALSE.
#' @param ... Additional parameters for internal functions (if applicable).
#' 
#' @return according to the call one of the following objects can be returned
#'  \item{show}{console output of the class and associated data.}
#'  \item{species}{character vector with the levels of Species if available.}
#'  \item{uniqueIds}{a DAVIDGenes object with only the gene names with a unique 
#'  id.}
#'  \item{duplicateIds}{a DAVIDGenes object with only the gene names with at 
#'  least two ids. If collapse is TRUE, a data.frame in where all the ids that
#'  matched the same gene name, are coded in comma separated style.}
#'  
#' @author Cristobal Fresno and Elmer A Fernandez
#'
#' @docType methods
#' @exportMethod species
#' @name species
#' @rdname DAVIDGenes-methods
#' @usage species(object)
#' @aliases species-methods
#' @examples
#' {
#' ##Load Show Gene List file report for the input demo file 1, using data
#' ##function. Then, create a DAVIDGenes object using the loaded data.frame
#' ##geneList1. In addition, the user can use the file name of the downloaded
#' ##file report.
#' data(geneList1)
#' davidGenes1<-DAVIDGenes(geneList1)
#' 
#' ##Now we can inspect davidGenes1 as it was an common data.frame
#' head(davidGenes1)
#' 
#' ##Additional getters for this object are also available, to obtain the
#' ##different columns: ids, genes and species.
#' ids(davidGenes1)
#' genes(davidGenes1)
#' species(davidGenes1)
#' 
#' ##Or even look up for a particular gene id, which will return only the
#' ##matched ones.
#' genes(davidGenes1, ids=c("38926_at", "35367_at", "no match"))
#' 
#' ##Obtain the genes with duplicate manufacturer ids or just the genes that
#' ##do not have duplicate ids (uniqueIds).
#' duplicateIds(davidGenes1)
#' uniqueIds(davidGenes1)
#' }
#'
setGeneric(name="species", def=function(object){standardGeneric("species")})
#'
#' @exportMethod species
#' @name species
#' @rdname DAVIDGenes-methods
#' @inheritParams species
#' @usage \S4method{species}{DAVIDGenes}(object)
#' @aliases species,DAVIDGenes-method
setMethod(f="species", signature= signature("DAVIDGenes"), 
  definition=function(object){
    ans<-character(0)
    if(nrow(object)>0){
      ans<-levels(object$Species)
    }
    return(ans)
})
#'
#' @exportMethod duplicateIds
#' @name duplicateIds
#' @rdname DAVIDGenes-methods
#' @inheritParams species
#' @usage duplicateIds(object, collapse = FALSE)
#' @aliases duplicateIds-methods
setGeneric(name="duplicateIds",def=function(object, collapse=FALSE){
  standardGeneric("duplicateIds")
})
#'
#' @exportMethod duplicateIds
#' @name duplicateIds
#' @rdname DAVIDGenes-methods
#' @inheritParams species
#' @usage \S4method{duplicateIds}{DAVIDGenes}(object, collapse=FALSE)
#' @aliases duplicateIds,DAVIDGenes-method
setMethod(f="duplicateIds", signature=signature("DAVIDGenes"), 
  definition=function(object, collapse=FALSE){
    ##Default output
    ans<-new("DAVIDGenes")
    ##Sanity check
    if(nrow(object)>0){
      name<-object$Name[duplicated(object$Name)]
      if(length(name)>0){
        if(collapse){
          ans<-cbind(ID=vector("character",length(name)),
            unique(object[object$Name%in%name, -1]))
          ans$ID<-sapply(ans$Name,function(x){
            toString(object$ID[object$Name==x])})
        }else{
          ans<-object[object$Name%in%name, ]
          ans<-as(ans, "DAVIDGenes")
          validObject(ans)
        }
      }
    }#nrow(object>0)
    return(ans)
})
#'
#' @exportMethod uniqueIds
#' @name uniqueIds
#' @rdname DAVIDGenes-methods
#' @inheritParams species
#' @usage uniqueIds(object)
#' @aliases uniqueIds-method
setGeneric(name="uniqueIds", def=function(object){standardGeneric("uniqueIds")})
#'
#' @exportMethod uniqueIds
#' @name uniqueIds
#' @rdname DAVIDGenes-methods
#' @inheritParams species
#' @usage \S4method{uniqueIds}{DAVIDGenes}(object)
#' @aliases uniqueIds,DAVIDGenes-method
setMethod(f="uniqueIds", signature=signature("DAVIDGenes"), 
  definition=function(object){
    ##Default output
    ans<-new("DAVIDGenes")
    ##Sanity check
    if(nrow(object)>0){
      ans<-as(object[!object$Name %in% object$Name[duplicated(object$Name)], ],
        "DAVIDGenes")
      validObject(ans)
    }
    return(ans)
})
