#' class "DAVIDFunctionalAnnotationChart" 
#'
#' This class represents the output of "Functional Annotation Chart" of DAVID.
#' It is an heir of DAVIDResult in the conceptual way, and also a data.frame
#' with additional features, such as identifying the unique and duplicate ids,
#' searching for genes with a given id, etc.  
#'
#' @section Type:
#' This class is a "\code{Concrete}" one.
#'
#' @section Extends:
#' \itemize{
#'  \item \emph{DAVIDResult} in the conceptual way.
#'  \item \emph{data.frame} in order to extend the basic features.
#' }
#'
#' @section Slots: no additional to the ones inherited from DAVIDResult and 
#' data.frame classes.
#'
#' @section Methods:
#' \describe{
#' \item{\code{show}}{\code{signature(object="DAVIDFunctionalAnnotationChart")}:
#' returns a basic console output.}
#' \item{\code{valid}}{\code{signature(object="DAVIDFunctionalAnnotationChart")}
#' : logical which checks DAVID's file output name ("Category", "Term",
#' "Count", etc.) presence.}
#' \item{\code{DAVIDFunctionalAnnotationChart}}{\code{signature(
#' object="character")}: constructor with the name of the .tab file report to
#' load.}
#' \item{\code{DAVIDFunctionalAnnotationChart}}{\code{signature(
#' object="data.frame")}: data.frame already loaded to use when constructing
#' the object.}
#' \item{\code{as}}{\code{signature(object="DAVIDFunctionalAnnotationChart")}:
#' coerce a data.frame into a DAVIDFunctionalAnnotationChart object.}
#' \item{\code{categories}}{\code{signature(
#'  object="DAVIDFunctionalAnnotationChart")}: obtain the factor vector of the
#' "Category" column.} 
#' \item{\code{ids}}{\code{signature(object="DAVIDFunctionalAnnotationChart")}:
#' obtain a list with character/integer vector with the ids of the corresponding
#' term.}
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
#' @keywords classes 
#' @family DAVIDFunctionalAnnotationChart
#' @name DAVIDFunctionalAnnotationChart-class
#' @rdname DAVIDFunctionalAnnotationChart-class
#' @exportClass DAVIDFunctionalAnnotationChart
#' @examples
#' {
#' ##Load the Functional Annotation Chart file report for the input demo 
#' ##file 2, using data function. Then, create a DAVIDFunctionalAnnotationChart
#' ## object using the loaded data.frame funChart2. In addition, the user can
#' ##use the file name of the downloaded file report.
#' data(funChart2)
#' davidFunChart2<-DAVIDFunctionalAnnotationChart(funChart2)
#' 
#' ##In Addition to the usual data.frame accessors, the user can inspect the
#' ##main categories used in the analysis. 
#' categories(davidFunChart2)
#' 
#' ##Obtain the ids of the genes present in each Term, as a list of character
#' ##vector
#' ids(davidFunChart2)
#' 
#' ##Or plot a 2D tile matrix with the reported evidence (green) or not (black).
#' ##Just to keep it simple, for the first five terms present in funChart2
#' ##object.
#'  plot2D(DAVIDFunctionalAnnotationChart(funChart2[1:5, ]),
#'    color=c("FALSE"="black", "TRUE"="green"))
#' }
#'
setClass(Class="DAVIDFunctionalAnnotationChart", 
  contains=c("DAVIDResult", "data.frame"),
  prototype=prototype(type="FunctionalAnnotationChart"),
  validity=function(object){
    valid<-TRUE
    if(nrow(object)>0){
     valid<-all(c("Category", "Term", "Count", "X.", "PValue", "Genes",
      "List.Total", "Pop.Hits", "Pop.Total", "Fold.Enrichment", "Bonferroni",
      "Benjamini", "FDR")%in%as.character(names(object)))
     if(!valid){print("Missing names in FunctionalAnnotationChart")}
    }
    return(valid)
})
