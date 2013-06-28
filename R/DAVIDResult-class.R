#' class "DAVIDResult" 
#'
#' This class represents the most generic result obtained in the \bold{D}atabase
#' for \bold{A}nnotation, \bold{V}isualization and \bold{I}ntegrated
#' \bold{D}iscovery (\bold{DAVID}) website (see References).
#'
#' @section Type:
#' This class is a "\code{Virtual}" one.
#'
#' @section Heirs:
#' \itemize{
#'  \item \code{DAVIDGenes}: basic gene information (ID, Name and Specie)
#'  \item \code{DAVIDCluster}: generic Cluster result (Term or Gene).
#'  \item \code{DAVIDFunctionalAnnotationChart}: EASE results on each 
#'  Functional Category (see references).
#'  \item \code{DAVIDFunctionalAnnotationTable}: annotation for each gene,
#'   no statistical analysis. 
#' }
#' 
#' @section Slots:
#' \describe{
#'  \item{\code{type}}{Object of class "character". Contains the name of 
#'  DAVID's result.}
#' }
#'
#' @section Methods:
#' \describe{
#'  \item{\code{show}}{\code{signature(object="DAVIDResult")}: returns a
#'  basic console output.}
#'  \item{\code{type}}{\code{signature(object="DAVIDResult")}: getter for 
#'  type slot.} 
#'  \item{\code{plot2D}}{\code{signature(object="DAVIDResult",
#'  dataFrame="data.frame")}: internal ggplot tile plot for gene/term cluster
#'  and annotation heirs.}
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
#'  \item Huang, D. W.; Sherman, B. T. & Lempicki, R. A. Bioinformatics 
#'  enrichment tools: paths toward the comprehensive functional analysis of
#'  large gene lists. Nucleic Acids Res, Laboratory of Immunopathogenesis and
#'  Bioinformatics, Clinical Services Program, SAIC-Frederick, Inc., National
#'  Cancer Institute at Frederick, Frede#rick, MD 21702, USA., 2009, 37, 1-13
#'  \item Xiaoli Jiao, Brad T. Sherman, Da Wei Huang, Robert Stephens, Michael 
#'  W. Baseler, H. Clifford Lane, Richard A. Lempicki, DAVID-WS: A Stateful Web
#'  Service to Facilitate Gene/Protein List Analysis Bioinformatics 2012
#'  doi:10.1093/bioinformatics/bts251
#' }
#'
#' @docType class
#' @keywords DAVID classes 
#' @family DAVIDResult
#' @name DAVIDResult-class
#' @rdname DAVIDResult-class
#' @exportClass DAVIDResult
setClass(Class="DAVIDResult", representation=representation(type="character",
  "VIRTUAL"))
