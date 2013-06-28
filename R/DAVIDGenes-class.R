#' class "DAVIDGenes" 
#'
#' This class represents the output of "Show Genes Result" of DAVID. It is an
#' heir of DAVIDResult in the conceptual way, and also a data.frame with
#' additional features, such as identifying the unique and duplicate ids,
#' searching for genes with a given id, etc.  
#'
#' @section Type:
#' This class is a "\code{Concrete}" one.
#'
#' @section Extends:
#' \itemize{
#'  \item \code{DAVIDResult} in the conceptual way.
#'  \item \code{data.frame} in order to extend the basic features.
#' }
#' 
#' @section Slots: none additional to the ones inherited from DAVIDResult and 
#' data.frame classes.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{valid}}{\code{signature(object="DAVIDGenes")}: logical which
#'  checks for data.frame name (ID, Name) presence.}
#' \item{\code{DAVIDGenes}}{\code{signature(object="character")}: constructor
#' with the name of the .tab file report to load.}
#' \item{\code{DAVIDGenes}}{\code{signature(object="data.frame")}: data.frame
#' already loaded to use when constructing the object.}
#' \item{\code{ids}}{\code{signature(object="DAVIDGenes")}: character vector
#' with gene submitted ids.}
#' }
#'
#' @author Cristobal Fresno and Elmer A Fernandez
#' 
#' @references 
#' \enumerate{
#'  \item The Database for Annotation, Visualization and Integrated Discovery 
#'  (david.abcc.ncifcrf.gov)
#'  \item Huang, D. W.; Sherman, B. T.; Tan, Q.; Kir, J.; Liu, D.; Bryant, D.; 
#'  Guo, Y.; Stephens, R.; Baseler, M. W.; Lane, H. C. & Lempicki, R. A. DAVID
#'  Bioinformatics Resources: expanded annotation database and novel algorithms
#'  to better extract biology from large gene lists. Nucleic Acids Res,
#'  Laboratory of Immunopathogenesis and Bioinformatics, SAIC-Frederick, Inc.,
#'  National Cancer Institute at Frederick, MD 21702, USA., 2007, 35, W169-W175
#' }
#'
#' @docType class
#' @keywords DAVID classes genes
#' @family DAVIDGenes
#' @name DAVIDGenes-class
#' @rdname DAVIDGenes-class
#' @exportClass DAVIDGenes
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
setClass(Class="DAVIDGenes", 
  contains=c("data.frame", "DAVIDResult"),
  prototype=prototype(type="Gene List Report"),
  validity=function(object){
    valid<-TRUE
    if(nrow(object)>0){
      valid<-all(c("ID", "Name")%in%as.character(names(object)))
      if(!valid){
        print("Missing ID or Name in names function call on DAVIDGenes class!")
      }
    }
    return(valid)
  }
)
